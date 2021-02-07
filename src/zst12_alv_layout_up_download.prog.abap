REPORT zst12_alv_layout_up_download.

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2021 Martin Preiss (PREISS Consulting)
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

*------------------------------------------------------------------------------*
* Tool for free usage, by Martin Preiss / PREISS CONSULTING
* Version 1.0  01/2021
*
* Tested with releases
* - ERP 6.0  (    (SAP_APPL 6.05 and SAP_APPL 6.17)
* - S/4 HANA 2020 (S4CORE 105)
* should work for other releases as well.
* Implemented without any newer ABAP features so it should work
* with older and newer releases.
*
* Download / Upload ALV Layouts
*
* - user-independent Layouts
* - user-dependent   Layouts
* - Layouts can be downloaded for a report and uploaded to the same
*   or to a different report in the same or in a different system
*
* # Summary
* SAP standard allows to transport user independent ALV layouts from
* one system to another.
* This reports should can help when you do not have the
* possibility to transport. It enables you to download
* or upload ALV variants for a report. It works for user-independent or user-dependent layouts.
* You can use it to:
* - copy ALV layouts from one system to another system
*   The system can have different release levels.
* - copy ALV layouts from one report to another
* - backup and restore ALV layouts
*
* # Selection Screen
*  - Report Name
*  - Local Directory for download / upload with F4 help
*  - Choice: Download or Upload
*
* # DOWNLOAD
* Popup with all existing ALV layouts for the report
* (user-independent and user-dependent)
* Displays Report, Handle, Log Group, layout, Description (in login language)
* User can select one or multiple layouts for download
* Saved in a group of files (Header data + texts, Field Cat, Sort, Filer, Layout)
* with file name REPORT_HANDLE_VARIANT_USERNAME_YYYYMMDD_SYSID_XXXX.txt
*   where XXXX
*    = desc for LTDXT texts (all languages)
*    = desc for Field catalog
*    = sort for Sort criteria (if maintained)
*    = filt for Filer criteria (if maintained)
* Special character '/' in the variant or handle is replaced by '#'
* in the file name.
*
* # UPLOAD
* Popup with all ALV layouts from download files
* in the specified local directory
* Displays Report, Handle, Log Group, layout, Description,
* Download date and system (derived from file name)
* User can select one or multiple layouts for upload
* ( user-independent and user-dependent)
* After succesful upload: Popup asking user if user independent layouts
* should be transported.
* If yes: standard dialog for customizing transport as in layout admin
*
* # Important Notes
*  - Upload is done to the specified target report.
*    No check if source and target report are equal( enables
*    copying variants from one report to another )
*  - Upload of user layout does not check user existence
*  - No authority check (except the check in the used SAP functions)
*  - No check at upload if layout already exists. It will be overwritten
*
* # Ideas for later improvements
* Upload:
* - Confirmation popup if layout already exists and will be overwritten ?
* - Check existence of user for user layouts ?
* - Check source vs. target report with confirmation popup if not equal ?
* - Target user parameter -> Would allow to copy User layouts to another user ?
* - existence check for report
*------------------------------------------------------------------------------*


" Target Report
PARAMETERS p_repid TYPE repid             OBLIGATORY.
" Local Directory for download / upload
PARAMETERS p_pathn TYPE sefs_d_path OBLIGATORY.

" Choice: Download oder Uplaod
PARAMETERS download TYPE xfeld RADIOBUTTON GROUP r1 DEFAULT 'X'.
PARAMETERS upload   TYPE xfeld RADIOBUTTON GROUP r1.

" TODO later?
" flag to explicitly allow/deny upload to different target report ?
" flag  to explicitly allow/deny upload to different target user
" parameter for Target User ?


*-----------------------------------------------------------------------------*
* processing logic
*-----------------------------------------------------------------------------*
CLASS alv1 DEFINITION FINAL.

  PUBLIC SECTION.

    " our structure for ALV variant processing
    TYPES BEGIN OF t_skeys.
    TYPES   selxx TYPE xfeld.  " result of selection dialog
    INCLUDE       TYPE ltdxt.  " Key and description in login language
    TYPES   filen TYPE string. " file name with Descriptions
    TYPES   aedat TYPE aedat.  " download date
    TYPES   sortf TYPE i.      " sort field
    TYPES   sysid TYPE sysid.  " source system
    " all descriptions, ( WITH EMPTY KEY just b/c non-generic table type needed)
    TYPES   ltdxt TYPE STANDARD TABLE OF ltdxt WITH DEFAULT KEY.
    TYPES   up_ok TYPE xfeld.  " =X after succesful upload
    TYPES END OF t_skeys.
    TYPES tt_skeys TYPE STANDARD TABLE OF t_skeys. "our ALV variant table

    CONSTANTS:
      c_invalid_char TYPE char9 VALUE '<>:"\|?*#',
      c_ftype        TYPE char4 VALUE '.txt'  ##NO_TEXT,
      c_suffix_fcat  TYPE char5 VALUE '_fcat' ##NO_TEXT,
      c_suffix_sort  TYPE char5 VALUE '_sort' ##NO_TEXT,
      c_suffix_filt  TYPE char5 VALUE '_filt' ##NO_TEXT,
      c_suffix_layo  TYPE char5 VALUE '_layo' ##NO_TEXT,
      c_suffix_desc  TYPE char5 VALUE '_desc' ##NO_TEXT.

    CLASS-DATA:
      gt_prot TYPE STANDARD TABLE OF sprot_u. "simple msg protocol

    CLASS-METHODS:
      main,
      download
        IMPORTING iv_repid TYPE repid
                  iv_pathn TYPE string,
      upload
        IMPORTING iv_repid TYPE repid
                  iv_pathn TYPE string,
      download_file
        IMPORTING iv_pathn TYPE string
                  iv_fname TYPE string
        CHANGING  ct_table TYPE STANDARD TABLE
                  cv_subrc TYPE sysubrc OPTIONAL,
      upload_file
        IMPORTING iv_pathn TYPE string
                  iv_fname TYPE string
        CHANGING  ct_table TYPE STANDARD TABLE
                  cv_subrc TYPE sysubrc OPTIONAL,
      f4_path
        CHANGING cv_path TYPE sefs_d_path,
      dynval_get
        IMPORTING iv_fname TYPE dynpread-fieldname
        CHANGING  cv_value TYPE any,
      variants_select
        IMPORTING iv_repid TYPE repid
                  iv_downl TYPE xfeld
        CHANGING  ct_skeys TYPE tt_skeys,
      protocol_display,
      msg_add.

ENDCLASS.

*-----------------------------------------------------------------------------*

CLASS alv1 IMPLEMENTATION.

  METHOD main.

    IF download EQ 'X'.   "download ALV layouts to local files
      download( iv_repid = p_repid
                iv_pathn = p_pathn ).
    ENDIF.

    IF upload EQ 'X'.     "upload ALV layouts from local files
      upload( iv_repid = p_repid
              iv_pathn = p_pathn ).
    ENDIF.

    protocol_display( ). " popup with messages

  ENDMETHOD.


  METHOD download.
*-----------------------------------------------------------------------------*
* download ALV layout to local files
*
* Popup with all existing ALV layouts for the report
* (user-independent and user-dependent)
* Displays Report, Handle, Log Group, layout, Description (in login language)
* User can select one or multiple layouts for download
* Saved in a group of files (Header data + texts, Field Cat, Sort, Filer, Layout)
* with file name REPORT_HANDLE_VARIANT_USERNAME_YYYYMMDD_SYSID_XXXX.txt
*   where XXXX
*    = desc for LTDXT texts (all languages)
*    = desc for Field catalog
*    = sort for Sort criteria (if maintained)
*    = filt for Filer criteria (if maintained)
* Special character '/' in the variant or handle is replaced by '#'
* in the file name.
*-----------------------------------------------------------------------------*
    DATA:
      ls_varkey     TYPE ltdxkey,
      lt_skeys      TYPE tt_skeys,
      lt_dbfieldcat TYPE STANDARD TABLE OF ltdxdata,
      lt_dbsortinfo TYPE STANDARD TABLE OF ltdxdata,
      lt_dbfilter   TYPE STANDARD TABLE OF ltdxdata,
      lt_dblayout   TYPE STANDARD TABLE OF ltdxdata,
      lt_ltdxt_all  TYPE SORTED TABLE OF ltdxt
                        WITH UNIQUE KEY handle log_group variant username type langu,
      lt_ltdxt      TYPE STANDARD TABLE OF ltdxt,
      ls_ltdxt      TYPE ltdxt,
      lv_namef      TYPE string,
      lv_filen      TYPE string,
      lv_subrc      TYPE sysubrc,
      lv_msgtx      TYPE msgtx ##NEEDED.

    FIELD-SYMBOLS:
      <skeys> TYPE t_skeys.

    " get layout headers -> Key Table LT_SKEYS
    SELECT * FROM ltdx
      INTO CORRESPONDING FIELDS OF TABLE lt_skeys
      WHERE relid  EQ 'LT'
        AND report EQ iv_repid
        AND srtf2  EQ 0.

    SORT lt_skeys BY log_group handle variant username.

    " read all layout descriptions for the report layouts
    SELECT * FROM ltdxt INTO TABLE lt_ltdxt_all
             WHERE relid  EQ 'LT'
               AND report EQ iv_repid.

    " build itab with layout and description in login language
    LOOP AT lt_skeys ASSIGNING <skeys>.
      MOVE-CORRESPONDING <skeys> TO ls_ltdxt.
      ls_ltdxt-langu = sy-langu.
      READ TABLE lt_ltdxt_all INTO ls_ltdxt FROM ls_ltdxt.
      CHECK sy-subrc EQ 0.
      <skeys>-langu = ls_ltdxt-langu.
      <skeys>-text  = ls_ltdxt-text.
    ENDLOOP.

    " let user select layouts for download
    variants_select( EXPORTING iv_repid = iv_repid
                               iv_downl = 'X'
                      CHANGING ct_skeys = lt_skeys ).

    READ TABLE lt_skeys TRANSPORTING NO FIELDS WITH KEY selxx = 'X'.
    IF sy-subrc NE 0.
      RETURN. "----------------------------------------> noting selected, exit
    ENDIF.

    " processing starts: set path into protocol
    " Path: &
    MESSAGE i160(ba) WITH iv_pathn INTO lv_msgtx.
    msg_add( ).

    " process selected layouts
    LOOP AT lt_skeys ASSIGNING <skeys> WHERE selxx = 'X'.

      CLEAR: lt_dbfieldcat, lt_dbsortinfo, lt_dbfilter, lt_dblayout,
             lt_ltdxt, ls_varkey.

      MOVE-CORRESPONDING <skeys> TO ls_varkey.

      " read ALV variant data
      CALL FUNCTION 'LT_DBDATA_READ_FROM_LTDX'
        EXPORTING
          is_varkey    = ls_varkey     " AlV layout key
        TABLES
          t_dbfieldcat = lt_dbfieldcat " field catalog   (always)
          t_dbsortinfo = lt_dbsortinfo " sort criteria   (optional)
          t_dbfilter   = lt_dbfilter   " filter criteria (optional)
          t_dblayout   = lt_dblayout   " layout settings (optional)
        EXCEPTIONS
          not_found    = 1
          wrong_relid  = 2
          OTHERS       = 3.

      IF sy-subrc <> 0. "should never be
        RETURN. "----------------------------------------------> exit method
      ENDIF.

      " get all language dependent descriptions for the variant
      LOOP AT lt_ltdxt_all INTO ls_ltdxt
           WHERE handle   EQ ls_varkey-handle
             AND variant  EQ ls_varkey-variant
             AND username EQ ls_varkey-username.
        APPEND ls_ltdxt TO lt_ltdxt.
      ENDLOOP.

*    check for reserved characters for Windows filenames
*
*    < (less than)
*    > (greater than)
*    : (colon)
*    " (double quote)
*    / (forward slash) -> used for user indep. variants -> we replace it by #
*    \ (backslash)
*    | (vertical bar or pipe)
*    ? (question mark)
*    * (asterisk)
*
* and # (collides with our replacement for / )
*
* We do not download layouts where the key contains any of these characters

      IF ls_varkey-handle CA c_invalid_char.
        " Invalid character &1 in &2
        MESSAGE e169(ccms_grmg) WITH ls_varkey-handle+sy-fdpos(1) ls_varkey-handle INTO lv_msgtx.
        msg_add( ).
        CONTINUE. " skip entry
      ENDIF.
      IF ls_varkey-variant CA c_invalid_char.
        " Invalid character &1 in &2
        MESSAGE e169(ccms_grmg) WITH ls_varkey-variant+sy-fdpos(1) ls_varkey-variant INTO lv_msgtx.
        msg_add( ).
        CONTINUE. " skip entry
      ENDIF.
      IF ls_varkey-username CA c_invalid_char.
        " Invalid character &1 in &2
        MESSAGE e169(ccms_grmg) WITH ls_varkey-username+sy-fdpos(1) ls_varkey-username INTO lv_msgtx.
        msg_add( ).
        CONTINUE. " skip entry
      ENDIF.

      " build base file name as HANDLE_VARIANT_USERNAME_YYYYMMDD_SYSID
      CONCATENATE ls_varkey-report
                  ls_varkey-handle
                  ls_varkey-variant
                  ls_varkey-username
                  sy-datlo
                  sy-sysid
                  INTO lv_namef SEPARATED BY '_'.

      " '/' as prefix for variants is a reserved char. in file names
      " -> replace it by #
      REPLACE ALL OCCURRENCES OF '/' IN lv_namef WITH '#'.

      " Download ALV layout
      " separate files for FieldCat, Sort, Filter, Layout and Description data

      IF lt_dbfieldcat IS NOT INITIAL.
        " build file name = base file name + Suffix + .txt
        lv_filen = lv_namef && c_suffix_fcat && c_ftype.
        download_file( EXPORTING iv_pathn = iv_pathn
                                 iv_fname = lv_filen
                        CHANGING ct_table = lt_dbfieldcat
                                 cv_subrc = lv_subrc  ).
        IF lv_subrc NE 0.
          RETURN. "---------------------------------> exit method
        ENDIF.
      ENDIF.

      IF lt_dbsortinfo IS NOT INITIAL.
        " build file name = base file name + Suffix + .txt
        lv_filen = lv_namef && c_suffix_sort && c_ftype.
        download_file( EXPORTING iv_pathn = iv_pathn
                                 iv_fname = lv_filen
                        CHANGING ct_table = lt_dbsortinfo
                                 cv_subrc = lv_subrc  ).
        IF lv_subrc NE 0.
          RETURN. "---------------------------------> exit method
        ENDIF.
      ENDIF.

      IF lt_dbfilter IS NOT INITIAL.
        " build file name = base file name + Suffix + .txt
        lv_filen = lv_namef && c_suffix_filt && c_ftype.
        download_file( EXPORTING iv_pathn = iv_pathn
                                 iv_fname = lv_filen
                        CHANGING ct_table = lt_dbfilter
                                 cv_subrc = lv_subrc  ).
        IF lv_subrc NE 0.
          RETURN. "---------------------------------> exit method
        ENDIF.
      ENDIF.

      IF lt_dblayout IS NOT INITIAL.
        " build file name = base file name + Suffix + .txt
        lv_filen = lv_namef && c_suffix_layo && c_ftype.
        download_file( EXPORTING iv_pathn = iv_pathn
                                 iv_fname = lv_filen
                        CHANGING ct_table = lt_dblayout
                                 cv_subrc = lv_subrc  ).
        IF lv_subrc NE 0.
          RETURN. "---------------------------------> exit method
        ENDIF.
      ENDIF.

      IF lt_ltdxt IS NOT INITIAL.
        " build file name = base file name + Suffix + .txt
        lv_filen = lv_namef && c_suffix_desc && c_ftype.
        download_file( EXPORTING iv_pathn = iv_pathn
                                 iv_fname = lv_filen
                        CHANGING ct_table = lt_ltdxt
                                 cv_subrc = lv_subrc  ).
        IF lv_subrc NE 0.
          RETURN. "---------------------------------> exit method
        ENDIF.
      ENDIF.

    ENDLOOP. "at lt_skeys

  ENDMETHOD.

  METHOD download_file.
*-----------------------------------------------------------------------------*
* local download for internal table
*-----------------------------------------------------------------------------*
    DATA:
      lv_filen TYPE string,
      lv_n     TYPE i,
      lv_msgtx TYPE msgtx ##NEEDED.

    lv_n = strlen( iv_pathn ) - 1.
    " build full filename, with path
    IF iv_pathn+lv_n(1) = '\'.
      lv_filen = iv_pathn && iv_fname.
    ELSE.
      lv_filen = iv_pathn && '\' && iv_fname.
    ENDIF.

    cl_gui_frontend_services=>gui_download(
        EXPORTING
          filename              = lv_filen
          filetype              = 'ASC'
          write_field_separator = 'X'
        CHANGING
          data_tab              = ct_table
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
           ).

    cv_subrc = sy-subrc.

    IF sy-subrc <> 0.
      " 165(SPRX) Error while downloading file '&1' &2
      MESSAGE e165(sprx) WITH iv_fname sy-subrc INTO lv_msgtx.
    ELSE.
      " Download complete (file: &1)
      MESSAGE s111(sr) WITH iv_fname INTO lv_msgtx.
    ENDIF.

    msg_add( ).

  ENDMETHOD.

  METHOD upload.
*-----------------------------------------------------------------------------*
* upload ALV layout from local files
*
* Popup with all ALV layouts from download files
* in the specified local directory
* Displays Report, Handle, Log Group, layout, Description,
* Download date and system (derived from file name)
* User can select one or multiple layouts for upload
* ( user-independent and user-dependent)
* After succesful upload: Popup asking user if user independent layouts
* should be transported.
* If yes: standard dialog for customizing transport as in layout admin
*
* # Important Notes
*  - Upload is done to the specified target report.
*    No check if source and target report are equal( enables
*    copying variants from one report to another )
*  - Upload of user layout does not check user existence
*  - No authority check (except the check in the used SAP functions)
*  - No check at upload if layout already exists. It will be overwritten
*-----------------------------------------------------------------------------*
    DATA:
      lt_dbfieldcat TYPE STANDARD TABLE OF ltdxdata,
      lt_dbsortinfo TYPE STANDARD TABLE OF ltdxdata,
      lt_dbfilter   TYPE STANDARD TABLE OF ltdxdata,
      lt_dblayout   TYPE STANDARD TABLE OF ltdxdata,
      lt_ltdxt      TYPE STANDARD TABLE OF ltdxt,
      ls_ltdxt      TYPE ltdxt,
      lt_ltdxkey    TYPE STANDARD TABLE OF ltdxkey,
      ls_ltdxkey    TYPE ltdxkey,
      lt_skeys      TYPE tt_skeys,
      ls_skeys      TYPE t_skeys,
      lv_subrc      TYPE sysubrc,
      lv_filter     TYPE string,
      lt_files      TYPE STANDARD TABLE OF char255,
      lv_file       TYPE char255,
      lv_text70     TYPE text70,
      lv_count      TYPE i,
      lv_pos        TYPE i,
      lv_answer     TYPE c,
      lv_msgtx      TYPE msgtx ##NEEDED,
      lv_filen      TYPE string.

    FIELD-SYMBOLS:
      <ltdxt> TYPE ltdxt,
      <skeys> TYPE t_skeys.

    lv_filter = '*' && c_suffix_desc && c_ftype.

    " get list 'Main' ALV files (*_desc.txt) from direcoty
    cl_gui_frontend_services=>directory_list_files(
      EXPORTING
        directory                   = iv_pathn
        filter                      = lv_filter  " *_desc.txt
        files_only                  = 'X'
      CHANGING
        file_table                  = lt_files
        count                       = lv_count
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        OTHERS                      = 6
           ).

    IF sy-subrc NE 0.
      " Method &1 was executed, sy-subrc = &2.
      MESSAGE e174(strex) WITH 'DIRECTORY_LIST_FILES' sy-subrc. " into lv_msgtx.
    ENDIF.

    " process *_desc.txt files (each represents one downloaded ALV layout)
    LOOP AT lt_files INTO lv_file.

      CLEAR: ls_skeys, lt_ltdxt, ls_ltdxt.

      " read keys and description from *_desc.txt file
      lv_filen = lv_file.
      upload_file( EXPORTING iv_pathn = iv_pathn
                             iv_fname = lv_filen
                   CHANGING  ct_table = lt_ltdxt
                             cv_subrc = lv_subrc ).

      IF lv_subrc NE 0.
        " Error while uploading file '&1' &2
        MESSAGE e164(sprx) WITH lv_filen lv_subrc.
        msg_add( ).
        CONTINUE.
      ENDIF.
      IF lt_ltdxt IS INITIAL.
        " should never be
        CONTINUE.
      ENDIF.

      " get Layout key and description
      READ TABLE lt_ltdxt INTO ls_ltdxt WITH KEY langu = sy-langu.
      IF sy-subrc NE 0.
        " fallback: use first other language
        READ TABLE lt_ltdxt INTO ls_ltdxt INDEX 1.
      ENDIF.

      MOVE-CORRESPONDING ls_ltdxt TO ls_skeys.

      CHECK ls_skeys-variant IS NOT INITIAL. " should always be

      " Keep Layout data in LT_SKEYS
      ls_skeys-filen = lv_file.
      " Keep descriptions
      ls_skeys-ltdxt = lt_ltdxt.
      " get upload data and source system from file name
      IF lv_file CP '*_20++++++_+++_*'.
        lv_pos = sy-fdpos + 1.
        ls_skeys-aedat = lv_filen+lv_pos(8).
        lv_pos = lv_pos + 9.
        ls_skeys-sysid = lv_filen+lv_pos(3).
      ENDIF.

      IF ls_skeys-report EQ iv_repid.
        ls_skeys-sortf = 1.
      ELSE.
        ls_skeys-sortf = 2.
      ENDIF.

      APPEND ls_skeys TO lt_skeys.

    ENDLOOP.  "at lt_files

    " sort variants: target report first,
    " variants with same key by download date (newest first)
    SORT lt_skeys BY sortf report
                     log_group handle
                     variant username
                     aedat DESCENDING.

    " let user select the layouts for upload (sets SELXX)
    variants_select( EXPORTING iv_repid = iv_repid
                               iv_downl = ' ' "upload mode
                     CHANGING  ct_skeys = lt_skeys ).

    " process selected layouts
    LOOP AT lt_skeys ASSIGNING <skeys> WHERE selxx = 'X'.

      CLEAR: lt_dbfieldcat, lt_dbsortinfo, lt_dbfilter, lt_dblayout.

      MOVE-CORRESPONDING <skeys> TO ls_ltdxkey.

      " get variant keys from LTDXT-table (all descr. have same variant key)
      READ TABLE <skeys>-ltdxt INTO ls_ltdxt INDEX 1.

      MOVE-CORRESPONDING ls_ltdxt TO ls_ltdxkey.

      " set target report ( we allow upload to different report)
      ls_ltdxkey-report = iv_repid.
      " adjust description keys
      LOOP AT <skeys>-ltdxt ASSIGNING <ltdxt>.
        <ltdxt>-report = iv_repid.
      ENDLOOP.

      " TODO later? check source = target report with confirmation popup
      " TODO later? check user existence for user layout

      lv_filen = <skeys>-filen.
      REPLACE c_suffix_desc IN lv_filen WITH c_suffix_fcat.
      upload_file( EXPORTING iv_pathn = iv_pathn
                             iv_fname = lv_filen
                   CHANGING  ct_table = lt_dbfieldcat
                             cv_subrc = lv_subrc ).
      IF lv_subrc <> 0. "Read Field catalog file ok? (always required)
        " Method &1 was executed, sy-subrc = &2.
        MESSAGE e174(strex) WITH 'GUI_UPLOAD' lv_subrc. " into lv_msgtx.
        " and abort processing
      ENDIF.

      IF lt_dbfieldcat IS INITIAL.
        " for safety, table should never be empty here
        CONTINUE.
      ENDIF.

      " read sort criteria from *_sort.txt file
      " ignore SUBRC, file doesnt need to exist
      lv_filen = <skeys>-filen.
      REPLACE c_suffix_desc IN lv_filen WITH c_suffix_sort.
      upload_file( EXPORTING iv_pathn = iv_pathn
                             iv_fname = lv_filen
                   CHANGING  ct_table = lt_dbsortinfo ).
      " read filter criteria from *_filt.txt file
      " ignore SUBRC, file doesnt need to exist
      lv_filen = <skeys>-filen.
      REPLACE c_suffix_desc IN lv_filen WITH c_suffix_filt.
      upload_file( EXPORTING iv_pathn = iv_pathn
                             iv_fname = lv_filen
                   CHANGING  ct_table = lt_dbfilter ).
      " read layout setting from *_layo.txt file
      " ignore SUBRC, file doesnt need to exist
      lv_filen = <skeys>-filen.
      REPLACE c_suffix_desc IN lv_filen WITH c_suffix_layo.
      upload_file( EXPORTING iv_pathn = iv_pathn
                             iv_fname = lv_filen
                    CHANGING ct_table = lt_dblayout ).

      " write layout to DB
      CALL FUNCTION 'LT_DBDATA_WRITE_TO_LTDX'
        EXPORTING
*         I_TOOL       = 'LT'
          is_varkey    = ls_ltdxkey
*         IS_VARIANT   = IS_VARIANT
        TABLES
          t_dbfieldcat = lt_dbfieldcat
          t_dbsortinfo = lt_dbsortinfo
          t_dbfilter   = lt_dbfilter
          t_dblayout   = lt_dblayout
        EXCEPTIONS
          not_found    = 1
          wrong_relid  = 2
          OTHERS       = 3.

      CHECK sy-subrc EQ 0. "should always be

      " insert/update descriptions to DB
      MODIFY ltdxt FROM TABLE <skeys>-ltdxt.

      " success message layout
      CONCATENATE ls_ltdxkey-handle  ls_ltdxkey-log_group
                  ls_ltdxkey-variant ls_ltdxkey-username
          INTO lv_text70 SEPARATED BY space.
      " Variant &1 saved
      MESSAGE s116(coat) WITH lv_text70 INTO lv_msgtx.
      msg_add( ).

      <skeys>-up_ok = 'X'. "set success flag

    ENDLOOP. " AT lt_skeys

    " set up table for transport
    LOOP AT lt_skeys INTO ls_skeys
      WHERE up_ok = 'X'
        AND username IS INITIAL.  "user dependent varinats are not transportable
      MOVE-CORRESPONDING ls_skeys TO ls_ltdxkey.
      ls_ltdxkey-type = '*'. "needed for transport
      APPEND ls_ltdxkey TO lt_ltdxkey.
    ENDLOOP. "at lt_skey INTO ls_skeys WHERE up_ok = 'X'.

    IF lt_ltdxkey IS INITIAL.
      RETURN. "--------------------------------> exit method
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Transport Layouts'(020)
        text_question  = 'Do you want to transport the uploaded user independent layouts?'(021)
        default_button = '2'  "No
*       display_cancel_button = 'X'
      IMPORTING
        answer         = lv_answer " to hold the FM's return value
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msgtx.
      msg_add( ).
    ENDIF.

    IF lv_answer NE '1'.
      RETURN. "--------------------------------> exit method
    ENDIF.

    CALL FUNCTION 'LT_VARIANTS_TRANSPORT'
      TABLES
        t_variants                  = lt_ltdxkey
      EXCEPTIONS
        client_unknown              = 1
        no_transports_allowed       = 2
        variants_table_empty        = 3
        no_transport_order_selected = 4
        OTHERS                      = 5.
    IF sy-subrc  EQ 1
    OR sy-subrc  EQ 2
    OR sy-subrc  EQ 5.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO lv_msgtx.
      msg_add( ).
    ENDIF.

  ENDMETHOD. "upload.


  METHOD upload_file.
*-----------------------------------------------------------------------------*
* upload local file to internal table
*-----------------------------------------------------------------------------*
    DATA:
      lv_filen TYPE string,
      lv_n     TYPE i.

    lv_n = strlen( iv_pathn ) - 1.
    " build full filename, with path
    IF iv_pathn+lv_n(1) = '\'.
      lv_filen = iv_pathn && iv_fname.
    ELSE.
      lv_filen = iv_pathn && '\' && iv_fname.
    ENDIF.

    " upload local file to internal table
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_filen
        filetype                = 'ASC'
        has_field_separator     = 'X'
*    has_field_separator     = SPACE
*    header_length           = 0
*    read_by_line            = 'X'
*    dat_mode                = SPACE
*    codepage                = SPACE
*    ignore_cerr             = ABAP_TRUE
*    replacement             = '#'
*    virus_scan_profile      = virus_scan_profile
*  IMPORTING
*    filelength              = filelength
*    header                  = header
      CHANGING
        data_tab                = ct_table
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19
           ).

    cv_subrc = sy-subrc.

* These messages might be useful:
*  240(ecatt) &1 uploaded successfully
*  164(SPRX)  Error while uploading file '&1' &2

  ENDMETHOD. "upload.

  METHOD f4_path.
*-----------------------------------------------------------------------------*
* let user select a local directory from application server
*-----------------------------------------------------------------------------*
    DATA:
       lv_dir    TYPE string.

    cl_gui_frontend_services=>directory_browse(
          EXPORTING initial_folder  = cv_path
          CHANGING  selected_folder = lv_dir ).

    IF lv_dir IS NOT INITIAL.
      cv_path = lv_dir.
    ENDIF.

  ENDMETHOD. "f4_path.

  METHOD dynval_get.
*-----------------------------------------------------------------------------*
* get fields from screen (important if
* F4 is called immediately without Enter
* after user changed screen values
*-----------------------------------------------------------------------------*
    DATA:
      ls_dynfd TYPE dynpread,
      lt_dynfd TYPE STANDARD TABLE OF dynpread.

    ls_dynfd-fieldname = iv_fname.
    APPEND ls_dynfd TO lt_dynfd.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
*       translate_to_upper   = 'X'
      TABLES
        dynpfields           = lt_dynfd
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.

    CHECK sy-subrc EQ 0.

    LOOP AT lt_dynfd INTO ls_dynfd
         WHERE fieldname = iv_fname.
      cv_value = ls_dynfd-fieldvalue.
    ENDLOOP.

* we use 'DYNP_VALUES_READ' in place of
* the shorter 'GET_DYNP_VALUE' to avoid
* UPPERCASE conversion.
* We want to see the file path in lowercase
*
*    CALL FUNCTION 'GET_DYNP_VALUE'
*      EXPORTING
*        i_field = iv_fname
*        i_repid = sy-repid
*        i_dynnr = sy-dynnr
*      CHANGING
*        o_value = cv_value.

  ENDMETHOD. "f4_path.


  METHOD protocol_display.
*-----------------------------------------------------------------------------*
*      protocol display (if not empty)
*-----------------------------------------------------------------------------*
    IF NOT gt_prot IS INITIAL.
      CALL FUNCTION 'SUSR_DISPLAY_LOG'
        EXPORTING
          display_in_popup = 'X'
        TABLES
          it_log_sprot     = gt_prot
        EXCEPTIONS
          parameter_error  = 0
          OTHERS           = 0.
    ENDIF.
  ENDMETHOD. "protocol_display


  METHOD msg_add.
*-----------------------------------------------------------------------------*
* add message from sy fields to protocol
*-----------------------------------------------------------------------------*
    DATA:
      ls_msg TYPE sprot_u.

    " message for Application Log
    ls_msg-severity  = sy-msgty.
    ls_msg-ag        = sy-msgid.
    ls_msg-msgnr     = sy-msgno.
    ls_msg-var1      = sy-msgv1.
    ls_msg-var2      = sy-msgv2.
    ls_msg-var3      = sy-msgv3.
    ls_msg-var4      = sy-msgv4.
    "  ls_msg-level =
    " ... see structure SPROT_U
    APPEND ls_msg TO gt_prot.
  ENDMETHOD. "msg_add.

  METHOD variants_select.
*-----------------------------------------------------------------------------*
* let user select ALV layouts from popup with a list of layouts
* different titles, field catalog and popup size for download/upload
*-----------------------------------------------------------------------------*
    DATA:
      ls_skeys TYPE t_skeys,
      lt_fdcat TYPE slis_t_fieldcat_alv,
      lv_endln TYPE i,
      lv_endcl TYPE i,
      lv_n     TYPE i,
      lv_max   TYPE i,
      lv_title TYPE text70.

    FIELD-SYMBOLS:
      <fdcat> TYPE slis_fieldcat_alv.

    " initialize field catalog based on DDIC
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'DISVARIANT'
      CHANGING
        ct_fieldcat            = lt_fdcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc NE 0. "should never be
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " get max lenght of reportname + 1
    LOOP AT ct_skeys INTO ls_skeys.
      lv_n = strlen( ls_skeys-report ) + 1.
      IF lv_n GT lv_max.
        lv_max = lv_n.
      ENDIF.
    ENDLOOP.

    IF iv_downl EQ 'X'.
      lv_title = ' - ' && iv_repid && ' - ' && 'Select Layouts for Download'(010).
      " modify field cat, changed field sequence, hide fields
      LOOP AT lt_fdcat ASSIGNING <fdcat>.
        <fdcat>-col_pos = sy-tabix.
        CASE <fdcat>-fieldname.
          WHEN 'VARIANT'.
            <fdcat>-col_pos = 1.
          WHEN 'HANDLE'.
            <fdcat>-col_pos = 2.
          WHEN 'LOG_GROUP'.
            <fdcat>-col_pos = 3.
          WHEN 'USERNAME'.
            <fdcat>-col_pos = 4.
          WHEN 'TEXT'.
            <fdcat>-col_pos = 5.
          WHEN 'REPORT'.
            " set display length of report name according to data
            <fdcat>-outputlen = lv_max.
            <fdcat>-col_pos = 6.
          WHEN OTHERS.
            <fdcat>-tech = 'X'.
        ENDCASE.
      ENDLOOP.

      lv_endcl = 100.

    ELSE. "upload
      " Title with report name and text
      lv_title = ' - ' && iv_repid && ' - ' && 'Select Layouts for Upload'(011).
      " modify field cat, changed field sequence, hide fields
      LOOP AT lt_fdcat ASSIGNING <fdcat>.
        <fdcat>-col_pos = sy-tabix.
        CASE <fdcat>-fieldname.
          WHEN 'VARIANT'.
            <fdcat>-col_pos = 1.
          WHEN 'HANDLE'.
            <fdcat>-col_pos = 2.
          WHEN 'LOG_GROUP'.
            <fdcat>-col_pos = 3.
          WHEN 'USERNAME'.
            <fdcat>-col_pos = 4.
          WHEN 'TEXT'.
            <fdcat>-col_pos = 5.
          WHEN 'REPORT'.
            " set display length of report name according to data
            <fdcat>-outputlen = lv_max.
            <fdcat>-col_pos = 6.
          WHEN OTHERS.
            <fdcat>-tech = 'X'.
        ENDCASE.
      ENDLOOP.

      APPEND INITIAL LINE TO lt_fdcat ASSIGNING <fdcat>.
      <fdcat>-fieldname = 'AEDAT'.
      <fdcat>-datatype  = 'DATS'.
      <fdcat>-seltext_s  =
      <fdcat>-seltext_m  =
      <fdcat>-seltext_l  = 'Downloaded'(012).
      <fdcat>-col_pos = 7.

      APPEND INITIAL LINE TO lt_fdcat ASSIGNING <fdcat>.
      <fdcat>-fieldname = 'SYSID'.
      <fdcat>-datatype  = 'CHAR'.
      <fdcat>-intlen     = 3.
      <fdcat>-outputlen  = 3.
      <fdcat>-seltext_s  =
      <fdcat>-seltext_m  =
      <fdcat>-seltext_l  = 'Sys'(013).
      <fdcat>-col_pos = 8.

      lv_endcl = 115.
    ENDIF.

    " calculate number of lines to display
    lv_endln = lines( ct_skeys ) + 3 + 3.
    IF lv_endln GT 20.
      lv_endln = 20.
    ENDIF.

    " Popup for select multiple lines
    " we set start/end coordinates. Otherwise popup is too narrow
    " and we have to scroll to see the full report name
    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING
        i_title               = lv_title
        i_tabname             = 'LT_SKEYS'
        i_checkbox_fieldname  = 'SELXX'     "Checkbox field defined in the internal table
        it_fieldcat           = lt_fdcat
        i_screen_start_column = 3
        i_screen_start_line   = 3
        i_screen_end_column   = lv_endcl  "better for us than default
        i_screen_end_line     = lv_endln
      TABLES
        t_outtab              = ct_skeys
      EXCEPTIONS
        OTHERS                = 0.

  ENDMETHOD. "variants_select.

ENDCLASS. "alv1

*-----------------------------------------------------------------------------*
* Selection Screen Events
*-----------------------------------------------------------------------------*

*-----------------------------------------------------------------------------*
INITIALIZATION.
*-----------------------------------------------------------------------------*
  " set default path to SAP Gui work directory
  cl_gui_frontend_services=>get_sapgui_workdir(
    CHANGING   sapworkdir = p_pathn
    EXCEPTIONS OTHERS     = 0 ).

*-----------------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pathn.
*-----------------------------------------------------------------------------*
  " field transport for F4 (important in case of change w/o Enter)
  alv1=>dynval_get( EXPORTING iv_fname = 'P_PATHN'
                    CHANGING  cv_value = p_pathn ).
  " let user select a local directory from application server
  alv1=>f4_path( CHANGING cv_path = p_pathn ).

*-----------------------------------------------------------------------------*
START-OF-SELECTION.
*-----------------------------------------------------------------------------*
  alv1=>main( ).
