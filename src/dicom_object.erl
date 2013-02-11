%% Copyright (c) 2013 Takaaki Uematsu <@plantavitpinus>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

-module(dicom_object).

-compile(export_all).

-export([
	read_file/1,
	write_file/2,
	value/3,
	value/2
	]).

-include_lib("eunit/include/eunit.hrl").

-include("dicom.hrl").

%% ===================================================================
%%  export functions

read_file(Name) ->
  case file:open(Name,[read,binary,raw]) of
   	{ok, Io} -> 
   	  Tag = read_tag_list(Io),
   	  file:close(Io),
   	  Tag;
  	{error, Reason} -> 
  	  {error, Reason}
  end.

write_file(Name, Obj) ->
  IoList = create_io_list(Obj),
  file:write_file(Name,IoList).

value(Obj, Tag) ->
  #tag{group=Group, element=Element} = Tag,
  value(Obj, Group, Element).

value(Obj, Group, Element) ->
  F = fun(A) -> is_same_tag(A, Group, Element) end,
  T = lists:filter(F, Obj),
  %?debugVal(T),
  #tag{value=Value} = erlang:hd(T),
  Value.

%% ===================================================================
%%  internal functions

is_same_tag(Obj, G, E) ->
  #tag{group=Group, element=Element} = Obj,
  case G of
    Group ->
      case E of
        Element -> true;
        _ -> false
      end;
    _ -> false
  end.

read_tag_list(Io) ->
  case is_dicom(Io) of
  	{ok, _} ->
  	  read_tag(Io, 128+4, []);
    {error, Reason} ->
      Reason
  end.

is_dicom(Io) ->
  {ok, Bin} = file:pread(Io, 0, 128+4),
  <<_:128/binary, Head:4/binary>> = Bin,
  %?debugVal(Head),
  is_dicom_head(Head).

is_dicom_head(<<"DICM">>) -> {ok, ""};
is_dicom_head(_) -> {error, "header not matched"}.

read_tag(Io, Pos, T) ->
  case file:pread(Io, Pos, 8) of
  	{ok, Bin} ->
      T_new = parse_tag(Io, Pos+8, Bin),
      %?debugVal(T_new),
      #tag{size=Size, exsize=ExSize} = T_new,
      read_tag(Io, Pos+8+Size+ExSize, T++[T_new]);
  	eof ->
      T
  end.

parse_tag(Io, Pos, <<Group:2/binary, Element:2/binary, 
	              Vr:2/binary, SizeBin:2/binary>>) ->
  %?debugVal([Group, Element]),
  [PosExt, Size, ExSize] = get_vr_size(Io, Pos, SizeBin, Vr),
  %?debugVal([PosExt,Size]),
  case ExSize of
    0 -> ReadSize = Size;
    _ -> ReadSize = ExSize
  end,
  case file:pread(Io, Pos+PosExt, ReadSize) of
    {ok, Value} ->
      #tag{group=b2i(Group), element=b2i(Element), 
        vr=b2s(Vr), size=Size, exsize=ExSize, value=Value};
    {error, Reason} ->
      ?debugVal([Size,ExSize]),
      Reason;
    eof ->
      ?debugVal([Size,ExSize]),
      eof
  end.

b2i(B) ->
  binary:decode_unsigned(B, little).
b2s(S) ->
  binary_to_list(S).


get_vr_size(SizeBin, VrBin) ->
  Size = b2i(SizeBin),
  ?debugVal([Size,VrBin]),
  case b2s(VrBin) of
    _ -> Size
  end.

get_vr_size(Io, Pos, SizeBin, VrBin) ->
  case b2s(VrBin) of
    A when A=:="OB"; A=:="OW"; A=:="OF"; A=:="SQ"; A=:="UT"; A=:="UN" ->
      {ok, ExtBin} = file:pread(Io, Pos, 4),
      %[4, b2i(SizeBin)+4, b2i(ExtBin)];
      [4, b2i(SizeBin)+4, b2i(ExtBin)];
    _ ->
      [0, b2i(SizeBin), 0]
  end.

create_io_list(TagList) ->
  Head = binary:copy(<<0>>,128),
  D = <<"DICM">>,
  Tag  = create_taglist(TagList),
  %?debugVal(Tag),
  [Head,D,Tag].

create_taglist([H|T]) ->
  L  = tagrecord2list(H),
  TL = create_taglist(T),
  L ++ TL;
create_taglist([]) ->
  [].

tagrecord2list(Tag) ->
  #tag{group=G, element=E, vr=Vr, size=Size, exsize=ExSize, value=Value} = Tag,
  %?debugVal([G,E]),
  [i2b(G,2),i2b(E,2),Vr,sizetag2bin(Size,ExSize),Value].

i2b(Bin,Size) ->
  BinSize = Size*8,
  lists:reverse(binary_to_list(<<Bin:BinSize>>)).

sizetag2bin(Size,ExSize) ->
  case ExSize of
    0 ->
      i2b(Size,2);
    _ ->
      [0, 0] ++ i2b(ExSize,4)
  end.

