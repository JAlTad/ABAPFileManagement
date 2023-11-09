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

    METHODS search_help_server_file IMPORTING i_default     TYPE eps2filnam
                                              i_only_path   TYPE flag
                                    RETURNING VALUE(r_path) TYPE eps2filnam.

    METHODS search_help_local_file RETURNING VALUE(r_path) TYPE eps2filnam.

    METHODS check_file_exists IMPORTING i_file_path   TYPE string
                                        i_file_name   TYPE string
                                        i_target      TYPE l_file_target
                              RETURNING VALUE(result) TYPE flag.

ENDCLASS.


CLASS z001_file_management IMPLEMENTATION.
*&---------------------------------------------------------------------*
*&      METHOD: SEARCH_HELP_SERVER_FILE_DES
*&---------------------------------------------------------------------*
*  DESC: Ayuda de búsqueda para fichero de servidor
*----------------------------------------------------------------------*
  METHOD search_help_server_file.
    CONSTANTS lc_path_symbol TYPE c LENGTH 1 VALUE '/'.

    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        directory        = i_default
*       FILEMASK         = ' '
      IMPORTING
        serverfile       = r_path
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ELSE.
      IF i_only_path = abap_true.
        DATA tab_dir_list TYPE STANDARD TABLE OF epsfili.
        CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
          EXPORTING
            dir_name               = CONV epsdirnam( r_path )
*           FILE_MASK              = ' '
*         IMPORTING
*           DIR_NAME               =
*           FILE_COUNTER           =
*           ERROR_COUNTER          =
          TABLES
            dir_list               = tab_dir_list
          EXCEPTIONS
            invalid_eps_subdir     = 1
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

*&---------------------------------------------------------------------*
*&      METHOD: SEARCH_LOCAL_SERVER_FILE
*&---------------------------------------------------------------------*
*  DESC: Ayuda de búsqueda para fichero local
*----------------------------------------------------------------------*
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
    DATA ld_file TYPE string.

    DATA file_separator TYPE c.

    CLEAR: ld_file,
           result.

    CASE i_target.

      WHEN server.
        file_separator = '/'.
        CONCATENATE i_file_path
                    file_separator
                    i_file_name INTO ld_file.

        OPEN DATASET ld_file FOR INPUT IN BINARY MODE.

        IF sy-subrc = 0.
          CLOSE DATASET ld_file.
          result = abap_true.
        ENDIF.

      WHEN local.

        cl_gui_frontend_services=>get_file_separator(
          CHANGING
            file_separator       = file_separator
          EXCEPTIONS
            not_supported_by_gui = 1
            error_no_gui         = 2
            cntl_error           = 3
            OTHERS               = 4
        ).
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

        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
