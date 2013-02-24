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

-define(SYNTAX_IMPLICIT_LITTLE, "1.2.840.10008.1.2").
-define(SYNTAX_EXPLICIT_LITTLE, "1.2.840.10008.1.2.1").
-define(SYNTAX_EXPLICIT_BIG,    "1.2.840.10008.1.2.2").

%% ===================================================================
%%  export functions

read_file(Name) ->
  case file:open(Name,[read,binary,raw]) of
   	{ok, Io} -> 
   	  Tag = read_dicom_tag_list(Io),
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

read_dicom_tag_list(Io) ->
  case is_dicom(Io) of
  	{ok, _} ->
  	  read_tag_list(Io, 128+4, explicit, []);
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

read_tag_list(Io, Pos, Syntax, T) ->
  case file:pread(Io, Pos, 4) of
  	{ok, Bin} ->
      [T_new, NewSyntax] = parse_tag(Io, Pos+4, Syntax, Bin),
      ?debugVal(T_new),
      #tag{size=Size, exsize=ExSize} = T_new,
      case ExSize of
        0 -> NewPos = Pos+8+Size+ExSize;
        _ -> NewPos = Pos+8+Size+ExSize+4
      end,
      read_tag_list(Io, NewPos, NewSyntax, T++[T_new]);
  	eof ->
      T
  end.

parse_tag(Io, Pos, Syntax, <<Group:2/binary, Element:2/binary>>) ->
  %?debugVal([Group, Element]),
  [Vr, PosExt, Size, ExSize] = read_vr_and_size(Io, Pos, Group, Element, Syntax),
  %?debugVal([b2s(Vr), PosExt, Size, ExSize, SyntaxSize]),
  case is_empty_tag(Size, ExSize) of
    true -> 
      [#tag{group=b2i(Group), element=b2i(Element), 
        vr=b2s(Vr), size=0, exsize=0}, Syntax];
    false ->
      T = parse_detail_tag(Io, Pos+4, Group, Element, Vr, Size, ExSize, PosExt),
      NewSyntax = read_transfer_syntax(T, Syntax),
      [T, NewSyntax]
  end.

read_vr_and_size(Io, Pos, Group, Element, Syntax) ->
  case get_transfer_syntax(b2i(Group), b2i(Element), Syntax) of
    explicit ->
      {ok, <<VrBin:2/binary, SizeBin:2/binary>>} = file:pread(Io, Pos, 4),

      case b2s(VrBin) of
        A when A=:="OB"; A=:="OW"; A=:="OF"; A=:="SQ"; A=:="UT"; A=:="UN" ->
          {ok, ExtBin} = file:pread(Io, Pos+4, 4),
          %[4, b2i(SizeBin)+4, b2i(ExtBin)];
          [VrBin, 4, b2i(SizeBin), b2i(ExtBin)];
        _ ->
          [VrBin, 0, b2i(SizeBin), 0]
      end;
    implicit ->
      {ok, SizeBin} = file:pread(Io, Pos, 4),
      [<<"implicit">>, 0, b2i(SizeBin), 0]
  end.

get_transfer_syntax(_, _, explicit) -> explicit;
get_transfer_syntax(2, _, implicit) -> explicit;
get_transfer_syntax(_, _, implicit) -> implicit.

read_transfer_syntax(#tag{group=16#02, element=16#10, value=Value}, _) ->
  SyntaxString = string:strip(b2s(Value),right,$0),
  ?debugVal(SyntaxString),
  case SyntaxString of
    ?SYNTAX_IMPLICIT_LITTLE -> implicit;
    ?SYNTAX_EXPLICIT_LITTLE -> explicit;
    ?SYNTAX_EXPLICIT_BIG    -> explicit;
    _ -> 
      ?debugVal("Unknown Transfer Syntax"),
      explicit
  end;
read_transfer_syntax(_, Syntax) -> Syntax.

parse_detail_tag(Io, Pos, Group, Element, Vr, Size, ExSize, PosExt) ->
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

is_empty_tag(Size, ExSize) ->
  case Size of
    0 ->
      case ExSize of
        0 -> true;
        _ -> false
      end;
    _ -> false
  end.

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

