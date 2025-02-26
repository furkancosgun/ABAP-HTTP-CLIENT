CLASS zcl_abap_http_client DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF content_types,
        form_urlencoded TYPE string VALUE 'application/x-www-form-urlencoded',
        json            TYPE string VALUE 'application/json',
        formdata        TYPE string VALUE 'multipart/form-data',
      END OF content_types.

    TYPES:
      BEGIN OF multipart_file,
        name         TYPE string,
        filename     TYPE string,
        content_type TYPE string,
        data         TYPE xstring,
      END OF multipart_file,

      multipart_files TYPE STANDARD TABLE OF multipart_file WITH EMPTY KEY.

    CLASS-METHODS create
      IMPORTING baseurl            TYPE string
                !path              TYPE string
                timeout            TYPE i          DEFAULT if_http_client=>co_timeout_default
                ssl_id             TYPE ssfapplssl OPTIONAL
      RETURNING VALUE(ro_instance) TYPE REF TO zcl_abap_http_client.

    METHODS get
      IMPORTING query_parameters TYPE tihttpnvp OPTIONAL
                header_fields    TYPE tihttpnvp OPTIONAL
      RETURNING VALUE(ro_client) TYPE REF TO zcl_abap_http_client.

    METHODS post
      IMPORTING body             TYPE string          OPTIONAL
                query_parameters TYPE tihttpnvp       OPTIONAL
                header_fields    TYPE tihttpnvp       OPTIONAL
                form_fields      TYPE tihttpnvp       OPTIONAL
                content_type     TYPE string          OPTIONAL
                multipart_files  TYPE multipart_files OPTIONAL
      RETURNING VALUE(ro_client) TYPE REF TO zcl_abap_http_client.

    METHODS put
      IMPORTING body             TYPE string          OPTIONAL
                query_parameters TYPE tihttpnvp       OPTIONAL
                header_fields    TYPE tihttpnvp       OPTIONAL
                form_fields      TYPE tihttpnvp       OPTIONAL
                content_type     TYPE string          OPTIONAL
                multipart_files  TYPE multipart_files OPTIONAL
      RETURNING VALUE(ro_client) TYPE REF TO zcl_abap_http_client.

    METHODS delete
      IMPORTING query_parameters TYPE tihttpnvp OPTIONAL
                header_fields    TYPE tihttpnvp OPTIONAL
      RETURNING VALUE(ro_client) TYPE REF TO zcl_abap_http_client.

    METHODS send
      EXPORTING  eo_response TYPE REF TO if_http_response
      EXCEPTIONS http_communication_failure.

    EVENTS before_request_sent EXPORTING VALUE(request) TYPE REF TO if_http_request.
    EVENTS after_request_sent EXPORTING VALUE(request) TYPE REF TO if_http_request.
    EVENTS response_received EXPORTING VALUE(client) TYPE REF TO if_http_client.

  PRIVATE SECTION.
    METHODS create_http_client.

    METHODS set_query_parameters
      IMPORTING query_parameters TYPE tihttpnvp.

    METHODS set_form_fields
      IMPORTING form_fields TYPE tihttpnvp.

    METHODS set_header_fields
      IMPORTING header_fields TYPE tihttpnvp.

    METHODS set_multipart_files
      IMPORTING multipart_files TYPE multipart_files.

    METHODS set_request_method
      IMPORTING !method TYPE string.

    METHODS set_content_type
      IMPORTING content_type TYPE string.

    METHODS set_request_body
      IMPORTING body TYPE string.

    METHODS prepare_http_request
      IMPORTING !method          TYPE string
                query_parameters TYPE tihttpnvp       OPTIONAL
                header_fields    TYPE tihttpnvp       OPTIONAL
                form_fields      TYPE tihttpnvp       OPTIONAL
                content_type     TYPE string          OPTIONAL
                multipart_files  TYPE multipart_files OPTIONAL
                body             TYPE string          OPTIONAL.

    DATA client   TYPE REF TO if_http_client.
    DATA url      TYPE string.
    DATA base_url TYPE string.
    DATA path     TYPE string.
    DATA timeout  TYPE i.
    DATA ssl_id   TYPE ssfapplssl.
ENDCLASS.


CLASS zcl_abap_http_client IMPLEMENTATION.
  METHOD create.
    ro_instance = NEW #( ).
    ro_instance->url      = baseurl && path.
    ro_instance->base_url = baseurl.
    ro_instance->path     = path.
    ro_instance->timeout  = timeout.
    ro_instance->ssl_id   = ssl_id.
  ENDMETHOD.

  METHOD create_http_client.
    cl_http_client=>create_by_url( EXPORTING url    = url
                                             ssl_id = ssl_id
                                   IMPORTING client = client ).
    client->propertytype_logon_popup   = client->co_disabled.
    client->propertytype_accept_cookie = client->co_enabled.
  ENDMETHOD.

  METHOD delete.
    prepare_http_request( method           = 'DELETE'
                          query_parameters = query_parameters
                          header_fields    = header_fields ).
    ro_client = me.
  ENDMETHOD.

  METHOD get.
    prepare_http_request( method           = 'GET'
                          query_parameters = query_parameters
                          header_fields    = header_fields ).
    ro_client = me.
  ENDMETHOD.

  METHOD post.
    prepare_http_request( method           = 'POST'
                          body             = body
                          query_parameters = query_parameters
                          header_fields    = header_fields
                          form_fields      = form_fields
                          content_type     = content_type
                          multipart_files  = multipart_files ).
    ro_client = me.
  ENDMETHOD.

  METHOD prepare_http_request.
    IF query_parameters IS NOT INITIAL.
      set_query_parameters( query_parameters = query_parameters ).
    ENDIF.

    create_http_client( ).

    set_request_method( method = method ).

    IF form_fields IS NOT INITIAL.
      set_form_fields( form_fields = form_fields ).
    ENDIF.

    IF header_fields IS NOT INITIAL.
      set_header_fields( header_fields = header_fields ).
    ENDIF.

    IF multipart_files IS NOT INITIAL.
      set_multipart_files( multipart_files = multipart_files ).
    ENDIF.

    IF content_type IS NOT INITIAL.
      set_content_type( content_type = content_type ).
    ENDIF.

    IF body IS NOT INITIAL.
      set_request_body( body = body ).
    ENDIF.
  ENDMETHOD.

  METHOD put.
    prepare_http_request( method           = 'PUT'
                          body             = body
                          query_parameters = query_parameters
                          header_fields    = header_fields
                          form_fields      = form_fields
                          content_type     = content_type
                          multipart_files  = multipart_files ).
    ro_client = me.
  ENDMETHOD.

  METHOD send.
    DATA message    TYPE string.
    DATA statuscode TYPE i.

    DEFINE raise_http_communication_fail.
      client->get_last_error( IMPORTING code    = statuscode
                                        message = message ).
      MESSAGE |{ statuscode }-{ message }| TYPE 'E' RAISING http_communication_failure.
    END-OF-DEFINITION.

    RAISE EVENT before_request_sent
      EXPORTING request = client->request.

    client->send( EXPORTING  timeout = timeout
                  EXCEPTIONS OTHERS  = 1 ).
    IF sy-subrc <> 0.
      raise_http_communication_fail.
    ENDIF.

    RAISE EVENT after_request_sent
      EXPORTING request = client->request.

    client->receive( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_http_communication_fail.
    ENDIF.

    RAISE EVENT response_received
      EXPORTING client = client.

    eo_response = client->response->copy( ).

    client->close( EXCEPTIONS OTHERS = 1 ).
  ENDMETHOD.

  METHOD set_content_type.
    client->request->set_content_type( content_type ).
  ENDMETHOD.

  METHOD set_form_fields.
    client->request->set_form_fields( fields = form_fields ).
  ENDMETHOD.

  METHOD set_header_fields.
    client->request->set_header_fields( fields = header_fields ).
  ENDMETHOD.

  METHOD set_multipart_files.
    LOOP AT multipart_files INTO DATA(file).
      DATA(lo_multipart) = client->request->add_multipart( ).
      DATA(lv_filename) = cl_http_utility=>escape_url( file-name ).
      lo_multipart->set_header_field(
          name  = 'content-disposition'
          value = |form-data; name="{ file-name }"; filename*=UTF-8''{ lv_filename }; filename="{ file-name }"| ).
      lo_multipart->set_content_type( content_type = file-content_type ).
      lo_multipart->set_data( file-data ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_query_parameters.
    LOOP AT query_parameters INTO DATA(query_parameter).
      cl_http_server=>if_http_server~append_field_url( EXPORTING name  = query_parameter-name
                                                                 value = query_parameter-value
                                                       CHANGING  url   = url ).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_request_body.
    client->request->set_cdata( body ).
  ENDMETHOD.

  METHOD set_request_method.
    client->request->set_method( method ).
  ENDMETHOD.
ENDCLASS.
