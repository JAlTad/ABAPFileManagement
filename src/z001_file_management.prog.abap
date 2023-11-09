*&---------------------------------------------------------------------*
*& Include z001_file_management
*& https://github.com/JAlTad/ABAPFileManagement.git
*& ABAP File management class
*&---------------------------------------------------------------------*


CLASS z001_file_management DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ENUM l_file_target BASE TYPE char1,
        undefined VALUE IS INITIAL,
        server    VALUE 'S',
        local     VALUE 'L',
      END OF ENUM l_file_target.

    "! Open a modal window to get filename or pathname
    "! @parameter i_default   | Input default pathname/filename
    "! @parameter i_only_path | Return only pathname
    "! @parameter r_path      | Result pathname or filename
    METHODS search_help_server_file IMPORTING i_default     TYPE eps2filnam
                                              i_only_path   TYPE flag
                                    RETURNING VALUE(r_path) TYPE eps2filnam.

    "! Open a modal window to select local file
    "! @parameter r_path | Complete filename with path
    METHODS search_help_local_file RETURNING VALUE(r_path) TYPE eps2filnam.

    "! Checks filename existence
    "! @parameter i_file_path    | Path
    "! @parameter i_file_name    | File name
    "! @parameter i_target       | (S)erver or (L)ocal
    "! @parameter result         | Existence flag result
    "! @exception file_not_found | File not found
    METHODS check_file_exists IMPORTING  i_file_path   TYPE string
                                         i_file_name   TYPE string
                                         i_target      TYPE l_file_target
                              RETURNING  VALUE(result) TYPE flag
                              EXCEPTIONS file_not_found.

    "! Creates an Excel (XLSX or MHTML) file form the internal table contents
    "! @parameter it_fieldcat | Field catalog
    "! @parameter it_sort     | Sort table
    "! @parameter it_filt     | Filter table
    "! @parameter is_layout   | Layout table
    "! @parameter i_xlsx      | XSLX ('X') or MHTML (' ') flag
    "! @parameter e_xstring   | XString content
    "! @parameter ct_data     | Table content
    METHODS create_xls_from_itab IMPORTING it_fieldcat TYPE lvc_t_fcat OPTIONAL
                                           it_sort     TYPE lvc_t_sort OPTIONAL
                                           it_filt     TYPE lvc_t_filt OPTIONAL
                                           is_layout   TYPE lvc_s_layo OPTIONAL
                                           i_xlsx      TYPE flag       OPTIONAL
                                 EXPORTING e_xstring   TYPE xstring
                                 CHANGING  ct_data     TYPE STANDARD TABLE.

    "! Read a file in binary mode and store the content in CT_DATA table
    "! @parameter i_filename | Filename and path
    "! @parameter i_target   | (S)erver or (L)ocal
    "! @parameter ct_data    | Binary file content
    "! @exception internal_error | Internal error
    METHODS read_binary_file IMPORTING  i_filename TYPE string
                                        i_target   TYPE l_file_target
                             CHANGING   ct_data    TYPE STANDARD TABLE
                             EXCEPTIONS internal_error.

  PRIVATE SECTION.
    METHODS read_binary_file_local IMPORTING  i_filename TYPE string
                                   CHANGING   ct_data    TYPE STANDARD TABLE
                                   EXCEPTIONS internal_error.

    METHODS read_binary_file_server IMPORTING  i_filename TYPE string
                                    CHANGING   ct_data    TYPE STANDARD TABLE
                                    EXCEPTIONS internal_error.
ENDCLASS.


CLASS z001_file_management IMPLEMENTATION.
  METHOD search_help_server_file.
    CONSTANTS lc_path_symbol TYPE c LENGTH 1 VALUE '/'.

    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING  directory        = i_default
*                 FILEMASK         = ' '
      IMPORTING  serverfile       = r_path
      EXCEPTIONS canceled_by_user = 1
                 OTHERS           = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ELSE.
      IF i_only_path = abap_true.
        DATA tab_dir_list TYPE STANDARD TABLE OF epsfili.
        CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
          EXPORTING  dir_name               = CONV epsdirnam( r_path )
*                     FILE_MASK              = ' '
*         IMPORTING
*                     DIR_NAME               =
*                     FILE_COUNTER           =
*                     ERROR_COUNTER          =
          TABLES     dir_list               = tab_dir_list
          EXCEPTIONS invalid_eps_subdir     = 1
                     sapgparam_failed       = 2
                     build_directory_failed = 3
                     no_authorization       = 4
                     read_directory_failed  = 5
                     too_many_read_errors   = 6
                     empty_directory_list   = 7
                     OTHERS                 = 8.
        IF sy-subrc = 0 OR sy-subrc = 7.

          " Es un directorio, vacío o no
          DATA(name_len) = strlen( r_path ).
          DATA(offset) = name_len - 1.

          IF r_path+offset(1) <> lc_path_symbol.
            CONCATENATE r_path lc_path_symbol INTO r_path.
          ENDIF.
        ELSE.
          MESSAGE s018(zcwt) WITH r_path DISPLAY LIKE 'E'.
* & no es un directorio válido

        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD search_help_local_file.
    DATA lv_rc         TYPE i.
    DATA lt_file_table TYPE filetable.
    DATA ls_file_path  LIKE LINE OF lt_file_table.

* Give file path
    cl_gui_frontend_services=>file_open_dialog( EXPORTING  " window_title            = CONV string( 'File' )
                                                " default_extension       = 'C:\'
*                                                           file_filter             = '*.txt'
                                                           multiselection          = abap_false
                                                CHANGING   file_table              = lt_file_table
                                                           rc                      = lv_rc " row count
*                                                           user_action             =
*                                                           file_encoding           =
                                                EXCEPTIONS file_open_dialog_failed = 1
                                                           cntl_error              = 2
                                                           error_no_gui            = 3
                                                           not_supported_by_gui    = 4
                                                           OTHERS                  = 5 ).

    IF sy-subrc = 0.
* in this point will pass the path to the parameter
      READ TABLE lt_file_table INDEX 1 INTO ls_file_path.
      IF sy-subrc = 0.
        r_path = ls_file_path-filename.
      ENDIF.
    ELSE.
* Error
    ENDIF.
  ENDMETHOD.

  METHOD check_file_exists.
    DATA ld_file        TYPE string.

    DATA file_separator TYPE c LENGTH 1.

    CLEAR: ld_file,
           result.

    CASE i_target.

      WHEN server.
        file_separator = '/'. " Unix-like separator
        CONCATENATE i_file_path
                    file_separator
                    i_file_name INTO ld_file.

        OPEN DATASET ld_file FOR INPUT IN BINARY MODE.

        IF sy-subrc = 0.
          CLOSE DATASET ld_file.
          result = abap_true.
        ELSE.
          RAISE file_not_found.
        ENDIF.

      WHEN local.

        cl_gui_frontend_services=>get_file_separator( CHANGING   file_separator       = file_separator
                                                      EXCEPTIONS not_supported_by_gui = 1
                                                                 error_no_gui         = 2
                                                                 cntl_error           = 3
                                                                 OTHERS               = 4 ).
        IF sy-subrc = 0.

          CONCATENATE i_file_path
                      file_separator
                      i_file_name INTO ld_file.

          cl_gui_frontend_services=>file_exist( EXPORTING  file                 = ld_file
                                                RECEIVING  result               = result
                                                EXCEPTIONS cntl_error           = 1
                                                           error_no_gui         = 2
                                                           wrong_parameter      = 3
                                                           not_supported_by_gui = 4
                                                           OTHERS               = 5 ).
          IF sy-subrc <> 0.
            RAISE file_not_found.
          ENDIF.

        ELSE.
          RAISE file_not_found.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD create_xls_from_itab.
**********************************************************************
* From http://abapblog.com/articles/tricks/33-create-xlsx-mhtml-file-from-internal-table-in-background
**********************************************************************

    DATA mt_fcat        TYPE lvc_t_fcat.
    DATA mt_data        TYPE REF TO data.
    DATA m_flavour      TYPE string.
    DATA m_version      TYPE string.
    DATA mo_result_data TYPE REF TO cl_salv_ex_result_data_table.
    DATA mo_columns     TYPE REF TO cl_salv_columns_table.
    DATA mo_aggreg      TYPE REF TO cl_salv_aggregations.
    DATA mo_salv_table  TYPE REF TO cl_salv_table.
    DATA m_file_type    TYPE salv_bs_constant.
    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    GET REFERENCE OF ct_data INTO mt_data.

    CLEAR e_xstring.

*if we didn't pass fieldcatalog we need to create it
    IF it_fieldcat[] IS INITIAL.
      ASSIGN mt_data->* TO <tab>.
      TRY.
          cl_salv_table=>factory( EXPORTING list_display = abap_false
                                  IMPORTING r_salv_table = mo_salv_table
                                  CHANGING  t_table      = <tab> ).
        CATCH cx_salv_msg INTO DATA(oref). " TODO: variable is assigned but never used (ABAP cleaner)

      ENDTRY.
      " get columns & aggregation infor to create fieldcat
      mo_columns = mo_salv_table->get_columns( ).
      mo_aggreg  = mo_salv_table->get_aggregations( ).
      mt_fcat    = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = mo_columns
                                                                      r_aggregations = mo_aggreg ).
    ELSE.
*else we take the one we passed
      mt_fcat[] = it_fieldcat[].
    ENDIF.

    IF    cl_salv_bs_a_xml_base=>get_version( ) = if_salv_bs_xml=>version_25
       OR cl_salv_bs_a_xml_base=>get_version( ) = if_salv_bs_xml=>version_26.

      mo_result_data = cl_salv_ex_util=>factory_result_data_table( r_data         = mt_data
                                                                   s_layout       = is_layout
                                                                   t_fieldcatalog = mt_fcat
                                                                   t_sort         = it_sort
                                                                   t_filter       = it_filt ).

      CASE cl_salv_bs_a_xml_base=>get_version( ).
        WHEN if_salv_bs_xml=>version_25.
          m_version = if_salv_bs_xml=>version_25.
        WHEN if_salv_bs_xml=>version_26.
          m_version = if_salv_bs_xml=>version_26.
      ENDCASE.

      " if we flag i_XLSX then we'll create XLSX if not then MHTML excel file
      IF i_xlsx IS NOT INITIAL.
        m_file_type = if_salv_bs_xml=>c_type_xlsx.
      ELSE.
        m_file_type = if_salv_bs_xml=>c_type_mhtml.
      ENDIF.

      m_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export.
      " transformation of data to excel
      cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform( EXPORTING xml_type      = m_file_type
                                                                  xml_version   = m_version
                                                                  r_result_data = mo_result_data
                                                                  xml_flavour   = m_flavour
                                                                  gui_type      = if_salv_bs_xml=>c_gui_type_gui
                                                        IMPORTING xml           = e_xstring ).
    ENDIF.
  ENDMETHOD.

  METHOD read_binary_file.
    CASE i_target.
      WHEN server.

        read_binary_file_server( EXPORTING  i_filename     = i_filename
                                 CHANGING   ct_data        = ct_data[]
                                 EXCEPTIONS internal_error = 1
                                            OTHERS         = 2 ).

        IF sy-subrc <> 0.
          RAISE internal_error.
        ENDIF.

      WHEN local.
        read_binary_file_local( EXPORTING  i_filename     = i_filename
                                CHANGING   ct_data        = ct_data[]
                                EXCEPTIONS internal_error = 1
                                           OTHERS         = 2 ).

        IF sy-subrc <> 0.
          RAISE internal_error.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD read_binary_file_local.
    FREE ct_data[].

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename                = i_filename
                                                     filetype                = 'BIN'
*                                                     has_field_separator     = space
*                                                     header_length           = 0
*                                                     read_by_line            = 'X'
*                                                     dat_mode                = space
*                                                     codepage                =
*                                                     ignore_cerr             = abap_true
*                                                     replacement             = '#'
*                                                     virus_scan_profile      =
*    IMPORTING
*                                                     filelength              =
*                                                     header                  =
                                          CHANGING   data_tab                = ct_data[]
*                                                     isscanperformed         = space
                                          EXCEPTIONS file_open_error         = 1
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
                                                     OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      RAISE internal_error.
    ENDIF.
  ENDMETHOD.

  METHOD read_binary_file_server.
    DATA l_reg TYPE x255.
    FIELD-SYMBOLS <hex_container> TYPE x.

    FREE ct_data[].

    OPEN DATASET i_filename FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      RAISE internal_error.
    ELSE.
* Read file and fill CT_DATA table

      ASSIGN l_reg TO <hex_container> CASTING.
      IF <hex_container> IS ASSIGNED.
        DO.
          CLEAR l_reg.
          READ DATASET i_filename INTO <hex_container>.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          APPEND l_reg TO ct_data.
        ENDDO.
      ENDIF.
      UNASSIGN <hex_container>.

      CLOSE DATASET i_filename.
      IF sy-subrc <> 0.
        RAISE internal_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
