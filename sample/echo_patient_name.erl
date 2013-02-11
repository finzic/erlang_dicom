-module(echo_patient_name).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-export([
  echo_patient_name/1
]).

-include("dicom.hrl").

echo_patient_name(Filename) ->
  Dobject = dicom_object:read_file(Filename),
  ?debugVal(dicom_object:value(Dobject, 16#10, 16#10)).

  %dicom_object:write_file("out.dcm", Dobject).
