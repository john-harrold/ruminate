# ruminate 0.3.0 (development version)

- Fix for rxode 3.0 release.
- Using new yaml save methodology for formods, and will break previous saved states
- Added preload() and mk_preload() files
- Updated `templates/ruminate.R` and `templates/ruminate_development.R` apps
  to use the preload file in the unzipped analysis state.


# ruminate 0.2.4 

- Fixed issue where changes in data views was leading to PH appearing in the 
  NCA data source selection
- Added sparse test dataset

# ruminate 0.2.3 

- Added default naming of NCA analysis from datasets
- Added searching in selection of NCA parameters
- Fixed sparse sampling in NCA
- Added CTS (Clinical trial simulator) module (in development)

# ruminate 0.2.2 

- Fixed error in default ruminate app causing it to crash
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
