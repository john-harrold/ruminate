# ruminate 0.2.2 (development version)

- Added CTS (Clinical trial simulator) module (in development)
- Separated ruminate.R sample app into two separate (the default one and one for developent.)

# ruminate 0.2.1 

- Fixed use of suggests that were not conditional. 

# ruminate 0.2.0 

- Added MB (model builder) module with (in development)
  - Support for rxode2 and NONMEM models
- Fixed bug in NCA module where column details didn't update properly when switching between analyses. 
- Fixed bug where detecting dosing from rows left the dosing records in the data frame for subsequent NCA.
- Fixed bug where the same parameter(s) were added over the same interval resulting in [[numeric]] in the reported values. Now if the same time interval is added more than once subsequent additions will update the parameters of the interval.
- Fixed stand-alone code generation and copy clipboard. 

# ruminate 0.1.1

- Initial release
