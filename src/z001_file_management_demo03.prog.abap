*&---------------------------------------------------------------------*
*& Report Z001_FILE_MANAGEMENT_DEMO03
*&---------------------------------------------------------------------*
*& https://github.com/JAlTad/ABAPFileManagement.git
*& ABAP File management class
*&---------------------------------------------------------------------*
*&
*& The MIT License (MIT)
*&
*& Copyright (c) 2023 ABAPFileManagement Contributors
*&
*& Permission is hereby granted, free of charge, to any person obtaining a copy
*& of this software and associated documentation files (the "Software"), to deal
*& in the Software without restriction, including without limitation the rights
*& to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*& copies of the Software, and to permit persons to whom the Software is
*& furnished to do so, subject to the following conditions:
*&
*& The above copyright notice and this permission notice shall be included in all
*& copies or substantial portions of the Software.
*&
*& THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*& IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*& FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*& AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*& LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*& OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*& SOFTWARE.
*&---------------------------------------------------------------------*
*& Description: Download Excel file in PC or server
*&---------------------------------------------------------------------*
REPORT z001_file_management_demo03.

INCLUDE z001_file_management.

*&---------------------------------------------------------------------*
*& GLOBAL DATA
*&---------------------------------------------------------------------*

DATA gcl_filemgr TYPE REF TO z001_file_management.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_table TYPE tabname DEFAULT 'T001' OBLIGATORY.
  PARAMETERS p_hdrtec TYPE c AS CHECKBOX. "Header with technical names
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002. " File selection
  PARAMETERS p_path TYPE eps2filnam OBLIGATORY.
  PARAMETERS: p_local TYPE c RADIOBUTTON GROUP g2 USER-COMMAND rad2,
              p_serv  TYPE c RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  IF p_local = abap_true.
    p_path = gcl_filemgr->search_help_local_file( EXPORTING i_only_path = abap_false ).
  ELSE.
    p_path = gcl_filemgr->search_help_server_file( EXPORTING i_default = '/'
                                                              i_only_path = abap_false ).
  ENDIF.

INITIALIZATION.
  PERFORM init.

START-OF-SELECTION.
  PERFORM process.

**********************************************************************

FORM process.
  DATA filename TYPE string.
  filename = p_path.
  PERFORM download_data USING p_table filename.
ENDFORM.

FORM init.
  DATA tmp_dir TYPE string.
  DATA file_sep TYPE c.

  p_local = abap_true.
  p_serv = abap_false.
  p_path = 'D:\'.

  IF gcl_filemgr IS INITIAL.
    gcl_filemgr = NEW z001_file_management( ).
  ENDIF.

  IF sy-batch IS INITIAL.
    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = file_sep
      EXCEPTIONS
        not_supported_by_gui = 1
        error_no_gui         = 2
        cntl_error           = 3
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    cl_gui_frontend_services=>get_sapgui_workdir(
      CHANGING
        sapworkdir            =  tmp_dir
      EXCEPTIONS
        get_sapworkdir_failed = 1                " Registry Error
        cntl_error            = 2                " Control error
        error_no_gui          = 3                " No GUI available
        not_supported_by_gui  = 4                " GUI does not support this
        OTHERS                = 5
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CONCATENATE tmp_dir file_sep p_table '-table.xlsx' INTO p_path.
    ENDIF.
  ENDIF.
ENDFORM.

FORM download_data USING p_table TYPE tabname
                         p_out TYPE string.

  CONSTANTS c_thresold TYPE i VALUE 100000. " max lines of selection table to manage per file

  FIELD-SYMBOLS <t_itab> TYPE STANDARD TABLE.
  DATA w_dref TYPE REF TO data.
  " -------------
  DATA slice_count TYPE numc4.
  DATA file_extension TYPE string.
  DATA filename_split TYPE string.
  DATA w_dref_slice TYPE REF TO data.
  FIELD-SYMBOLS <t_itab_slice> TYPE STANDARD TABLE.
  " -------------

  CREATE DATA w_dref TYPE TABLE OF (p_table).
  ASSIGN w_dref->* TO <t_itab>.

  CHECK <t_itab> IS ASSIGNED.

  " Read data from Table (Full)
  SELECT *
    FROM (p_table)
    INTO TABLE <t_itab>.

  IF sy-subrc = 0.

    " Split file if table has more lines than C_THRESOLD
    IF lines( <t_itab> ) <= c_thresold.
      PERFORM download_file USING <t_itab> p_out.
    ELSE.

      CREATE DATA w_dref_slice TYPE TABLE OF (p_table).
      ASSIGN w_dref_slice->* TO <t_itab_slice>.

      CHECK <t_itab_slice> IS ASSIGNED.

      WHILE lines( <t_itab> ) > c_thresold.
        FREE <t_itab_slice>.
        ADD 1 TO slice_count.
        PERFORM get_slice_filename USING p_out slice_count CHANGING filename_split.

        " Copy subset and delete records from main table
        LOOP AT <t_itab> ASSIGNING FIELD-SYMBOL(<item>) FROM 1 TO c_thresold.
          APPEND <item> TO <t_itab_slice>.
        ENDLOOP.

        DELETE <t_itab> FROM 1 TO c_thresold.
        IF sy-subrc = 0.
          PERFORM download_file USING <t_itab_slice> filename_split.
        ENDIF.
      ENDWHILE.

      " Last subset
      IF lines( <t_itab> ) > 0.
        ADD 1 TO slice_count.
        PERFORM get_slice_filename USING p_out slice_count CHANGING filename_split.
        PERFORM download_file USING <t_itab> filename_split.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<T_ITAB>  text
*      -->P_P_OUT  text
*----------------------------------------------------------------------*
FORM download_file  USING    p_itab TYPE ANY TABLE
                             p_out TYPE string.

  DATA xls_xstring TYPE xstring.
  DATA bin_tab TYPE TABLE OF x.
**********************************************************************
* Crear field catalog
  DATA mt_fcat        TYPE lvc_t_fcat.
  DATA mo_result_data TYPE REF TO cl_salv_ex_result_data_table.
  DATA mo_columns     TYPE REF TO cl_salv_columns_table.
  DATA mo_aggreg      TYPE REF TO cl_salv_aggregations.
  DATA mo_salv_table  TYPE REF TO cl_salv_table.

  TRY.
      cl_salv_table=>factory( EXPORTING list_display = abap_false
                              IMPORTING r_salv_table = mo_salv_table
                              CHANGING  t_table      = p_itab ).
    CATCH cx_salv_msg INTO DATA(oref).

  ENDTRY.
  " get columns & aggregation infor to create fieldcat
  mo_columns = mo_salv_table->get_columns( ).
  mo_aggreg  = mo_salv_table->get_aggregations( ).
  mt_fcat    = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = mo_columns
                                                                  r_aggregations = mo_aggreg ).

  " Header with technical names instead of descriptions
  IF p_hdrtec = abap_true.
    LOOP AT mt_fcat ASSIGNING FIELD-SYMBOL(<fcat_item>).
      <fcat_item>-reptext = <fcat_item>-fieldname.
      <fcat_item>-scrtext_s = <fcat_item>-fieldname.
      <fcat_item>-scrtext_m = <fcat_item>-fieldname.
      <fcat_item>-scrtext_l = <fcat_item>-fieldname.
    ENDLOOP.
  ENDIF.
**********************************************************************

  gcl_filemgr->create_xls_from_itab(
    EXPORTING
    it_fieldcat = mt_fcat
*      it_sort     =
*      it_filt     =
*      is_layout   =
      i_xlsx      = 'X'
    IMPORTING
      e_xstring   =  xls_xstring
    CHANGING
      ct_data     = p_itab
  ).

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = xls_xstring
*     append_to_table = space
*    IMPORTING
*     output_length   =
    TABLES
      binary_tab = bin_tab.

  IF p_local = abap_true.
    cl_gui_frontend_services=>gui_download(
      EXPORTING
*    bin_filesize              =                      " File length for binary files
        filename                  = p_out                     " Name of file
        filetype                  = 'BIN'                " File type (ASCII, binary ...)
*    append                    = space                " Character Field of Length 1
*    write_field_separator     = space                " Separate Columns by Tabs in Case of ASCII Download
*    header                    = '00'                 " Byte Chain Written to Beginning of File in Binary Mode
*    trunc_trailing_blanks     = space                " Do not Write Blank at the End of Char Fields
*    write_lf                  = 'X'                  " Insert CR/LF at End of Line in Case of Char Download
*    col_select                = space                " Copy Only Selected Columns of the Table
*    col_select_mask           = space                " Vector Containing an 'X' for the Column To Be Copied
*    dat_mode                  = space                " Numeric and date fields are in DAT format in WS_DOWNLOAD
*    confirm_overwrite         = space                " Overwrite File Only After Confirmation
*    no_auth_check             = space                " Switch off Check for Access Rights
*    codepage                  =                      " Character Representation for Output
*    ignore_cerr               = abap_true            " Ignore character set conversion errors?
*    replacement               = '#'                  " Replacement Character for Non-Convertible Characters
*    write_bom                 = space                " If set, writes a Unicode byte order mark
*    trunc_trailing_blanks_eol = 'X'                  " Remove Trailing Blanks in Last Column
*    wk1_n_format              = space
*    wk1_n_size                = space
*    wk1_t_format              = space
*    wk1_t_size                = space
*    show_transfer_status      = 'X'                  " Enables suppression of transfer status message
*    fieldnames                =                      " Table Field Names
*    write_lf_after_last_line  = 'X'                  " Writes a CR/LF after final data record
*    virus_scan_profile        = '/SCET/GUI_DOWNLOAD' " Virus Scan Profile
*  IMPORTING
*    filelength                =                      " Number of bytes transferred
      CHANGING
        data_tab                  =   bin_tab                   " Transfer table
      EXCEPTIONS
        file_write_error          = 1                    " Cannot write to file
        no_batch                  = 2                    " Cannot execute front-end function in background
        gui_refuse_filetransfer   = 3                    " Incorrect Front End
        invalid_type              = 4                    " Invalid value for parameter FILETYPE
        no_authority              = 5                    " No Download Authorization
        unknown_error             = 6                    " Unknown error
        header_not_allowed        = 7                    " Invalid header
        separator_not_allowed     = 8                    " Invalid separator
        filesize_not_allowed      = 9                    " Invalid file size
        header_too_long           = 10                   " Header information currently restricted to 1023 bytes
        dp_error_create           = 11                   " Cannot create DataProvider
        dp_error_send             = 12                   " Error Sending Data with DataProvider
        dp_error_write            = 13                   " Error Writing Data with DataProvider
        unknown_dp_error          = 14                   " Error when calling data provider
        access_denied             = 15                   " Access to File Denied
        dp_out_of_memory          = 16                   " Not enough memory in data provider
        disk_full                 = 17                   " Storage medium is full.
        dp_timeout                = 18                   " Data provider timeout
        file_not_found            = 19                   " Could not find file
        dataprovider_exception    = 20                   " General Exception Error in DataProvider
        control_flush_error       = 21                   " Error in Control Framework
        not_supported_by_gui      = 22                   " GUI does not support this
        error_no_gui              = 23                   " GUI not available
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSEIF p_serv = abap_true.
    OPEN DATASET p_out FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      WRITE : / 'ERROR OPENING FILE :', p_out COLOR COL_NEGATIVE.
      STOP.
    ENDIF.

    LOOP AT bin_tab ASSIGNING FIELD-SYMBOL(<itab_line>).
      TRANSFER <itab_line> TO p_out.
    ENDLOOP.

***close file p_otfile.
    CLOSE DATASET p_out.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SLICE_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OUT  text
*      <--P_FILENAME_SPLIT  text
*----------------------------------------------------------------------*
FORM get_slice_filename  USING    p_out TYPE string
                                  p_slice_count TYPE numc4
                         CHANGING p_filename_split TYPE string.

  DATA file_extension TYPE string.

  CLEAR file_extension.
  CALL FUNCTION 'CH_SPLIT_FILENAME'
    EXPORTING
      complete_filename = p_out                 " File Name to be split
*     check_dos_format  =                  " Check For DOS Format
    IMPORTING
*     drive             =                  " The disk drive specification (without colon)
      extension         = file_extension                 " The file name extension (extension)
*     name              =                  " The file name (without extension)
*     name_with_ext     =                  " The file name (with extension)
*     path              =                  " The path (without disk drive and file name)
    EXCEPTIONS
      invalid_drive     = 1                " Incorrect disk drive specification
      invalid_path      = 2                " Incorrect path
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ELSE.
    p_filename_split = p_out.

    IF file_extension IS NOT INITIAL.
      CONCATENATE '.' file_extension INTO file_extension.
      REPLACE file_extension IN p_filename_split WITH ''.
      CONCATENATE p_filename_split '-' p_slice_count file_extension INTO p_filename_split.
    ELSE.
      CONCATENATE p_filename_split '-' p_slice_count INTO p_filename_split.
    ENDIF.

  ENDIF.

ENDFORM.
