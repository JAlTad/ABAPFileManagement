*&---------------------------------------------------------------------*
*& Report Z001_FILE_MANAGEMENT_DEMO01
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
REPORT z001_file_management_demo01.

INCLUDE z001_file_management.

*&---------------------------------------------------------------------*
*& GLOBAL DATA
*&---------------------------------------------------------------------*

DATA gcl_filemgr TYPE REF TO z001_file_management.

*&---------------------------------------------------------------------*
*& PARAMETERS
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001. "Path selection
  SELECTION-SCREEN COMMENT /1(79) text-c01. "Even selecting a file, returns its path
  PARAMETERS p_path1 TYPE eps2filnam OBLIGATORY.
  PARAMETERS: p_local1 TYPE c RADIOBUTTON GROUP g1 USER-COMMAND rad1,
              p_serv1  TYPE c RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002. "File selection
  PARAMETERS p_path2 TYPE eps2filnam OBLIGATORY.
  PARAMETERS: p_local2 TYPE c RADIOBUTTON GROUP g2 USER-COMMAND rad2,
              p_serv2  TYPE c RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK b2.
*&---------------------------------------------------------------------*
*& INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
  p_local1 = abap_true.
  p_serv1 = abap_false.
  p_path1 = 'C:/'.

  p_local2 = abap_true.
  p_serv2 = abap_false.
  p_path2 = 'C:/'.

  IF gcl_filemgr IS INITIAL.
    gcl_filemgr = NEW z001_file_management( ).
  ENDIF.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path1.
  IF p_local1 = abap_true.
    p_path1 = gcl_filemgr->search_help_local_file( EXPORTING i_only_path = abap_true  ).
  ELSE.
    p_path1 = gcl_filemgr->search_help_server_file( EXPORTING i_default = '/'
                                                              i_only_path = abap_true ).
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path2.
  IF p_local2 = abap_true.
    p_path2 = gcl_filemgr->search_help_local_file( EXPORTING i_only_path = abap_false ).
  ELSE.
    p_path2 = gcl_filemgr->search_help_server_file( EXPORTING i_default = '/'
                                                              i_only_path = abap_false ).
  ENDIF.
