# SAP_ALV_LAYOUT_DOWNLOAD_UPLOAD
Report ZST12_ALV_LAYOUT_UP_DOWNLOAD to download and upload ALV layouts

free for anyone to use, modify, and run on their own, under MIT licence.

Tool for free usage, by Martin Preiss / PREISS CONSULTING
Version 1.0  01/2021

documentation with screen shots   
www.preiss-sap-consulting.de/en/download-und-upload-von-alv-layouts/ (english)   
www.preiss-sap-consulting.de/download-und-upload-von-alv-layouts/ (german)

Tested with releases
- ERP 6.0  (    (SAP_APPL 6.05 and SAP_APPL 6.17)
- S/4 HANA 2020 (S4CORE 105)
should work for other releases as well.
Implemented without any newer ABAP features so it should work
with older and newer releases.

Download / Upload ALV Layouts

- user-independent Layouts
- user-dependent   Layouts
- Layouts can be downloaded for a report and uploaded to the same
  or to a different report in the same or in a different system

# Summary
SAP standard allows to transport user independent ALV layouts from
one system to another.
This reports should can help when you do not have the
possibility to transport. It enables you to download
or upload ALV variants for a report. It works for user-independent or user-dependent layouts.
You can use it to:
- copy ALV layouts from one system to another system
  The system can have different release levels.
- copy ALV layouts from one report to another
- backup and restore ALV layouts

# Selection Screen
 - Report Name
 - Local Directory for download / upload with F4 help
 - Choice: Download or Upload

# DOWNLOAD
Popup with all existing ALV layouts for the report
(user-independent and user-dependent)
Displays Report, Handle, Log Group, layout, Description (in login language)
User can select one or multiple layouts for download
Saved in a group of files (Header data + texts, Field Cat, Sort, Filer, Layout)
with file name REPORT_HANDLE_VARIANT_USERNAME_YYYYMMDD_SYSID_XXXX.txt
  where XXXX
   = desc for LTDXT texts (all languages)
   = desc for Field catalog
   = sort for Sort criteria (if maintained)
   = filt for Filer criteria (if maintained)
Special character '/' in the variant or handle is replaced by '#'
in the file name.

# UPLOAD
Popup with all ALV layouts from download files
in the specified local directory
Displays Report, Handle, Log Group, layout, Description,
Download date and system (derived from file name)
User can select one or multiple layouts for upload
( user-independent and user-dependent)
After succesful upload: Popup asking user if user independent layouts
should be transported.
If yes: standard dialog for customizing transport as in layout admin

# Important Notes
 - Upload is done to the specified target report.
   No check if source and target report are equal( enables
   copying variants from one report to another )
 - Upload of user layout does not check user existence
 - No authority check (except the check in the used SAP functions)
 - No check at upload if layout already exists. It will be overwritten

# Ideas for later improvements
Upload:
- Confirmation popup if layout already exists and will be overwritten ?
- Check existence of user for user layouts ?
- Check source vs. target report with confirmation popup if not equal ?
- Target user parameter -> Would allow to copy User layouts to another user ?
- existence check for report

