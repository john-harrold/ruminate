# ruminate 0.2.0 
- Added MB (model builder) module with
  - Support for rxode2 and NONMEM models
- Fixed bug in NCA module where column details didn't update properly when switching between analyses. 
- Fixed bug where detecting dosing from rows left the dosing records in the data frame for subsequent NCA.
- Fixed bug where the same parameter(s) were added over the same interval resulting in [[numeric]] in the reported values. Now if the same time interval is added more than once subsequent additions will update the parameters of the interval.

# ruminate 0.1.1
- Initial release
