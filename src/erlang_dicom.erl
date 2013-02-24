-module(erlang_dicom).

-export([start/0]).

start() ->
  echo_patient_name:echo_patient_name("./sample.dcm"),
  %echo_patient_name:echo_patient_name("./sample_im.dcm"),
  %write_file:write_file("./sample.dcm", "./out.dcm"),
  ok.
