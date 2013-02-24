## About

erlang_dicom is a simple DICOM library for erlang.
please see /sample directory.

	1> Dcm_obj = erlang_dicom:read_file("sample.dcm"),
	2> Patientname = Dcm_obj.value(16#10, 16#10),
	<<"Anonymized">>
	...

Dcm_obj is a list of "tag" records. please see "dicom.hrl" for details.
Currently, file read/write I/F with explicit/implicit VR can be supported.

## TODO

 * Add support for Big Endian
 * Add support for SQ tag
 * Add support for network communication

## Contribution

* Takaaki Uematsu
