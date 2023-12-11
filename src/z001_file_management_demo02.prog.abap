*&---------------------------------------------------------------------*
*& Report Z001_FILE_MANAGEMENT_DEMO02
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
REPORT z001_file_management_demo02.

INCLUDE z001_file_management.

*&---------------------------------------------------------------------*
*& GLOBAL DATA
*&---------------------------------------------------------------------*

DATA gcl_filemgr TYPE REF TO z001_file_management.

*&---------------------------------------------------------------------*
*& PARAMETERS
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002. "File selection
  PARAMETERS p_path TYPE eps2filnam OBLIGATORY.
  PARAMETERS: p_local TYPE c RADIOBUTTON GROUP g2 USER-COMMAND rad2,
              p_serv  TYPE c RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK b2.
*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.

  p_local = abap_true.
  p_serv = abap_false.
  p_path = 'C:/'.

  IF gcl_filemgr IS INITIAL.
    gcl_filemgr = NEW z001_file_management( ).
  ENDIF.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*&---------------------------------------------------------------------*


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  IF p_local = abap_true.
    p_path = gcl_filemgr->search_help_local_file( EXPORTING i_only_path = abap_false ).
  ELSE.
    p_path = gcl_filemgr->search_help_server_file( EXPORTING i_default = '/'
                                                              i_only_path = abap_false ).
  ENDIF.

START-OF-SELECTION.

  DATA tab_string TYPE STANDARD TABLE OF string.
  gcl_filemgr->read_text_file(
    EXPORTING
      i_filename     = CONV string( p_path )
      i_target       = COND #( WHEN p_local = abap_true AND p_serv = abap_false THEN gcl_filemgr->local
                               WHEN p_local = abap_false AND p_serv = abap_true THEN gcl_filemgr->server )
    CHANGING
      ct_data        = tab_string[]
EXCEPTIONS
  internal_error = 1
  invalid_target = 2
  OTHERS         = 3
  ).
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
* Display content
    cl_demo_output=>display( data = tab_string[] ).
  ENDIF.
