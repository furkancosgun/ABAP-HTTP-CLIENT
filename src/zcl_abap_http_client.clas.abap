CLASS zcl_abap_http_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    CONSTANTS:
      " Define standard content types for HTTP requests
      BEGIN OF content_types,
        form_urlencoded TYPE string VALUE 'application/x-www-form-urlencoded',
        json            TYPE string VALUE 'application/json',
        formdata        TYPE string VALUE 'multipart/form-data',
        xml             TYPE string VALUE 'application/xml',
        plain_text      TYPE string VALUE 'text/plain',
        html            TYPE string VALUE 'text/html',
        csv             TYPE string VALUE 'text/csv',
        pdf             TYPE string VALUE 'application/pdf',
        zip             TYPE string VALUE 'application/zip',
        jpeg            TYPE string VALUE 'image/jpeg',
        png             TYPE string VALUE 'image/png',
        gif             TYPE string VALUE 'image/gif',
      END OF content_types.

    TYPES:
      " Type definition for multipart file
      BEGIN OF ty_multipart_file,
        name         TYPE string,
        filename     TYPE string,
        content_type TYPE string,
        data         TYPE xstring,
      END OF ty_multipart_file,

      " Table type for storing multiple multipart files
      ty_multipart_files TYPE STANDARD TABLE OF ty_multipart_file WITH EMPTY KEY.

    CLASS-METHODS:
      " Factory method to create an instance of the HTTP client
      create
        IMPORTING
          baseurl            TYPE string
          path               TYPE string
          timeout            TYPE i DEFAULT if_http_client=>co_timeout_default
          ssl_id             TYPE ssfapplssl OPTIONAL
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_abap_http_client.

    METHODS:
      " Perform a GET request
      get
        IMPORTING
          query_parameters TYPE tihttpnvp OPTIONAL
          header_fields    TYPE tihttpnvp OPTIONAL
          content_type     TYPE string DEFAULT zcl_abap_http_client=>content_types-json
        RETURNING
          VALUE(ro_client) TYPE REF TO zcl_abap_http_client,

      " Perform a POST request
      post
        IMPORTING
          body             TYPE string OPTIONAL
          query_parameters TYPE tihttpnvp OPTIONAL
          header_fields    TYPE tihttpnvp OPTIONAL
          form_fields      TYPE tihttpnvp OPTIONAL
          content_type     TYPE string DEFAULT zcl_abap_http_client=>content_types-json
          multipart_files  TYPE ty_multipart_files OPTIONAL
        RETURNING
          VALUE(ro_client) TYPE REF TO zcl_abap_http_client,

      " Perform a PUT request
      put
        IMPORTING
          body             TYPE string OPTIONAL
          query_parameters TYPE tihttpnvp OPTIONAL
          header_fields    TYPE tihttpnvp OPTIONAL
          form_fields      TYPE tihttpnvp OPTIONAL
          content_type     TYPE string DEFAULT zcl_abap_http_client=>content_types-json
          multipart_files  TYPE ty_multipart_files OPTIONAL
        RETURNING
          VALUE(ro_client) TYPE REF TO zcl_abap_http_client,

      " Perform a DELETE request
      delete
        IMPORTING
          query_parameters TYPE tihttpnvp OPTIONAL
          header_fields    TYPE tihttpnvp OPTIONAL
          content_type     TYPE string DEFAULT zcl_abap_http_client=>content_types-json
        RETURNING
          VALUE(ro_client) TYPE REF TO zcl_abap_http_client,

      " Send the HTTP request and handle the response
      send
        EXPORTING
          eo_response TYPE REF TO if_http_response
        EXCEPTIONS
          http_communication_failure.

    EVENTS:
      " Event triggered before the request is sent
      before_request_sent EXPORTING VALUE(request) TYPE REF TO if_http_request,

      " Event triggered after the request is sent
      after_request_sent EXPORTING VALUE(request) TYPE REF TO if_http_request,

      " Event triggered when the response is received
      response_received EXPORTING VALUE(client) TYPE REF TO if_http_client.

  PRIVATE SECTION.
    METHODS:
      " Create an HTTP client instance
      create_http_client,

      " Set query parameters for the request
      set_query_parameters
        IMPORTING
          query_parameters TYPE tihttpnvp,

      " Set form fields for the request
      set_form_fields
        IMPORTING
          form_fields TYPE tihttpnvp,

      " Set header fields for the request
      set_header_fields
        IMPORTING
          header_fields TYPE tihttpnvp,

      " Add multipart files to the request
      set_multipart_files
        IMPORTING
          multipart_files TYPE ty_multipart_files,

      " Set the request method (GET, POST, PUT, DELETE)
      set_request_method
        IMPORTING
          method TYPE string,

      " Set the content type of the request
      set_content_type
        IMPORTING
          content_type TYPE string,

      " Set the request body
      set_request_body
        IMPORTING
          body TYPE string,

      " Prepare the HTTP request with various settings
      prepare_http_request
        IMPORTING
          method           TYPE string
          query_parameters TYPE tihttpnvp OPTIONAL
          header_fields    TYPE tihttpnvp OPTIONAL
          form_fields      TYPE tihttpnvp OPTIONAL
          content_type     TYPE string OPTIONAL
          multipart_files  TYPE ty_multipart_files OPTIONAL
          body             TYPE string OPTIONAL.

    DATA:
      client   TYPE REF TO if_http_client, " HTTP client instance
      url      TYPE string,       " Complete URL for the request
      base_url TYPE string,       " Base URL
      path     TYPE string,       " Path of the request
      timeout  TYPE i,            " Timeout duration
      ssl_id   TYPE ssfapplssl.   " SSL ID for secure connections
ENDCLASS.



CLASS ZCL_ABAP_HTTP_CLIENT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_HTTP_CLIENT=>CREATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] BASEURL                        TYPE        STRING
* | [--->] PATH                           TYPE        STRING
* | [--->] TIMEOUT                        TYPE        I (default =IF_HTTP_CLIENT=>CO_TIMEOUT_DEFAULT)
* | [--->] SSL_ID                         TYPE        SSFAPPLSSL(optional)
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_ABAP_HTTP_CLIENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create.
    " Initialize an instance of the HTTP client class
    ro_instance = NEW #( ).
    ro_instance->url      = baseurl && path.
    ro_instance->base_url = baseurl.
    ro_instance->path     = path.
    ro_instance->timeout  = timeout.
    ro_instance->ssl_id   = ssl_id.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_HTTP_CLIENT->CREATE_HTTP_CLIENT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_http_client.
    " Create an HTTP client instance by URL
    cl_http_client=>create_by_url(
      EXPORTING
        url                = me->url
        ssl_id             = me->ssl_id
      IMPORTING
        client             = me->client
    ).
    me->client->propertytype_logon_popup   = me->client->co_disabled.
    me->client->propertytype_accept_cookie = me->client->co_enabled.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_HTTP_CLIENT->DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] QUERY_PARAMETERS               TYPE        TIHTTPNVP(optional)
* | [--->] HEADER_FIELDS                  TYPE        TIHTTPNVP(optional)
* | [--->] CONTENT_TYPE                   TYPE        STRING (default =ZCL_ABAP_HTTP_CLIENT=>CONTENT_TYPES-JSON)
* | [<-()] RO_CLIENT                      TYPE REF TO ZCL_ABAP_HTTP_CLIENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD delete.
    " Prepare and send a DELETE request
    prepare_http_request(
      method           = 'DELETE'
      query_parameters = query_parameters
      header_fields    = header_fields
      content_type     = content_type
    ).
    ro_client = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_HTTP_CLIENT->GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] QUERY_PARAMETERS               TYPE        TIHTTPNVP(optional)
* | [--->] HEADER_FIELDS                  TYPE        TIHTTPNVP(optional)
* | [--->] CONTENT_TYPE                   TYPE        STRING (default =ZCL_ABAP_HTTP_CLIENT=>CONTENT_TYPES-JSON)
* | [<-()] RO_CLIENT                      TYPE REF TO ZCL_ABAP_HTTP_CLIENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get.
    " Prepare and send a GET request
    prepare_http_request(
      method           = 'GET'
      query_parameters = query_parameters
      header_fields    = header_fields
      content_type     = content_type
    ).
    ro_client = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_HTTP_CLIENT->POST
* +-------------------------------------------------------------------------------------------------+
* | [--->] BODY                           TYPE        STRING(optional)
* | [--->] QUERY_PARAMETERS               TYPE        TIHTTPNVP(optional)
* | [--->] HEADER_FIELDS                  TYPE        TIHTTPNVP(optional)
* | [--->] FORM_FIELDS                    TYPE        TIHTTPNVP(optional)
* | [--->] CONTENT_TYPE                   TYPE        STRING (default =ZCL_ABAP_HTTP_CLIENT=>CONTENT_TYPES-JSON)
* | [--->] MULTIPART_FILES                TYPE        TY_MULTIPART_FILES(optional)
* | [<-()] RO_CLIENT                      TYPE REF TO ZCL_ABAP_HTTP_CLIENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD post.
    " Prepare and send a POST request
    prepare_http_request(
      method           = 'POST'
      body             = body
      query_parameters = query_parameters
      header_fields    = header_fields
      form_fields      = form_fields
      content_type     = content_type
      multipart_files  = multipart_files
    ).
    ro_client = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_HTTP_CLIENT->PREPARE_HTTP_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] METHOD                         TYPE        STRING
* | [--->] QUERY_PARAMETERS               TYPE        TIHTTPNVP(optional)
* | [--->] HEADER_FIELDS                  TYPE        TIHTTPNVP(optional)
* | [--->] FORM_FIELDS                    TYPE        TIHTTPNVP(optional)
* | [--->] CONTENT_TYPE                   TYPE        STRING(optional)
* | [--->] MULTIPART_FILES                TYPE        TY_MULTIPART_FILES(optional)
* | [--->] BODY                           TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD prepare_http_request.
    " Set query parameters
    set_query_parameters( query_parameters = query_parameters ).

    " Create HTTP client instance
    create_http_client( ).

    " Set form fields (if any)
    set_form_fields( form_fields = form_fields ).

    " Set header fields (if any)
    set_header_fields( header_fields = header_fields ).

    " Add multipart files (if any)
    set_multipart_files( multipart_files = multipart_files ).

    " Set the HTTP method
    set_request_method( method = method ).

    " Set the content type of the request
    set_content_type( content_type = content_type ).

    " Set the request body (if any)
    set_request_body( body = body ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_HTTP_CLIENT->PUT
* +-------------------------------------------------------------------------------------------------+
* | [--->] BODY                           TYPE        STRING(optional)
* | [--->] QUERY_PARAMETERS               TYPE        TIHTTPNVP(optional)
* | [--->] HEADER_FIELDS                  TYPE        TIHTTPNVP(optional)
* | [--->] FORM_FIELDS                    TYPE        TIHTTPNVP(optional)
* | [--->] CONTENT_TYPE                   TYPE        STRING (default =ZCL_ABAP_HTTP_CLIENT=>CONTENT_TYPES-JSON)
* | [--->] MULTIPART_FILES                TYPE        TY_MULTIPART_FILES(optional)
* | [<-()] RO_CLIENT                      TYPE REF TO ZCL_ABAP_HTTP_CLIENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD put.
    " Prepare and send a PUT request
    prepare_http_request(
      method           = 'PUT'
      body             = body
      query_parameters = query_parameters
      header_fields    = header_fields
      form_fields      = form_fields
      content_type     = content_type
      multipart_files  = multipart_files
    ).
    ro_client = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_HTTP_CLIENT->SEND
* +-------------------------------------------------------------------------------------------------+
* | [<---] EO_RESPONSE                    TYPE REF TO IF_HTTP_RESPONSE
* | [EXC!] HTTP_COMMUNICATION_FAILURE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send.
    DATA:
      message    TYPE string,
      statuscode TYPE i.

    " Define a macro to handle HTTP communication failure
    DEFINE raise_http_communication_fail.
      me->client->get_last_error(
        IMPORTING
          code           = statuscode
          message        = message
      ).
      MESSAGE |{ statuscode }-{ message }| TYPE 'E' RAISING http_communication_failure.
    END-OF-DEFINITION.

    " Trigger event before the request is sent
    RAISE EVENT before_request_sent EXPORTING request = me->client->request.

    " Send the HTTP request
    me->client->send(
      EXPORTING
        timeout                    = me->timeout
      EXCEPTIONS
        OTHERS                     = 1
    ).
    IF sy-subrc <> 0.
      raise_http_communication_fail.
    ENDIF.

    " Trigger event after the request is sent
    RAISE EVENT after_request_sent EXPORTING request = me->client->request.

    " Receive the HTTP response
    me->client->receive(
      EXCEPTIONS
        OTHERS                     = 1
    ).
    IF sy-subrc <> 0.
      raise_http_communication_fail.
    ENDIF.

    " Trigger event when the response is received
    RAISE EVENT response_received EXPORTING client = me->client.

    " Get a response copy before closing
    eo_response = me->client->response->copy( ).

    " Close connection
    me->client->close(
      EXCEPTIONS
        OTHERS                     = 1
    ).
    IF sy-subrc <> 0.
      raise_http_communication_fail.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_HTTP_CLIENT->SET_CONTENT_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] CONTENT_TYPE                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_content_type.
    " Set the content type of the request
    client->request->set_content_type( content_type ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_HTTP_CLIENT->SET_FORM_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] FORM_FIELDS                    TYPE        TIHTTPNVP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_form_fields.
    " Set form fields for the request
    client->request->set_form_fields(
      EXPORTING
        fields = form_fields
    ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_HTTP_CLIENT->SET_HEADER_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] HEADER_FIELDS                  TYPE        TIHTTPNVP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_header_fields.
    " Set header fields for the request
    client->request->set_header_fields(
      EXPORTING
        fields = header_fields
    ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_HTTP_CLIENT->SET_MULTIPART_FILES
* +-------------------------------------------------------------------------------------------------+
* | [--->] MULTIPART_FILES                TYPE        TY_MULTIPART_FILES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_multipart_files.
    " Add multipart files to the request
    LOOP AT multipart_files INTO DATA(file).
      DATA(lo_multipart) = client->request->add_multipart( ).
      DATA(lv_filename) = cl_http_utility=>escape_url( file-name ).
      lo_multipart->set_header_field(
        EXPORTING
          name  = 'content-disposition'
          value = |form-data; name="{ file-name }"; filename*=UTF-8''{ lv_filename }; filename="{ file-name }"|
      ).
      lo_multipart->set_content_type( content_type = file-content_type ).
      lo_multipart->set_data( file-data ).
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_HTTP_CLIENT->SET_QUERY_PARAMETERS
* +-------------------------------------------------------------------------------------------------+
* | [--->] QUERY_PARAMETERS               TYPE        TIHTTPNVP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_query_parameters.
    " Append query parameters to the URL
    LOOP AT query_parameters INTO DATA(query_parameter).
      cl_http_server=>if_http_server~append_field_url(
        EXPORTING
          name  = query_parameter-name
          value = query_parameter-value
        CHANGING
          url   = me->url
      ).
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_HTTP_CLIENT->SET_REQUEST_BODY
* +-------------------------------------------------------------------------------------------------+
* | [--->] BODY                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_request_body.
    " Set the body of the request
    IF body IS NOT INITIAL.
      client->request->set_cdata( body ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_HTTP_CLIENT->SET_REQUEST_METHOD
* +-------------------------------------------------------------------------------------------------+
* | [--->] METHOD                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_request_method.
    " Set the HTTP request method (GET, POST, PUT, DELETE)
    client->request->set_method( method ).
  ENDMETHOD.
ENDCLASS.
