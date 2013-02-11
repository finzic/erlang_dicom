-module(write_file).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-export([
  write_file/2
]).

-include("dicom.hrl").

write_file(InFilename, OutFilename) ->
  Dobject = dicom_object:read_file(InFilename),
  dicom_object:write_file(OutFilename, Dobject).
