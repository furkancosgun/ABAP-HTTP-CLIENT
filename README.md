# ZABAP_HTTP_CLIENT

This project demonstrates the use of a custom HTTP client class for handling HTTP requests and responses in ABAP. The provided examples cover various HTTP operations including GET, POST, PUT, DELETE requests, and multipart file uploads.

## License
This project is licensed under the [MIT License](LICENSE). See the LICENSE file for details.

## Contents
- [GET Request Example](#get-request-example)
- [POST Request with JSON Data](#post-request-with-json-data)
- [PUT Request with Form Data](#put-request-with-form-data)
- [DELETE Request Example](#delete-request-example)
- [Multipart File Upload](#multipart-file-upload)
- [POST Request with Authorization Header](#post-request-with-authorization-header)
- [HTTP Client Handler Class](#http-client-handler-class)

### HTTP Client Handler Class
Example of defining a class to handle HTTP client events:
```abap
CLASS zcl_http_client_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " Event handler for before request is sent
      on_before_request_sent
        FOR EVENT before_request_sent OF zcl_abap_http_client
        IMPORTING sender request,

      " Event handler for after request is sent
      on_after_request_sent
        FOR EVENT after_request_sent OF zcl_abap_http_client
        IMPORTING sender request,

      " Event handler for when the response is received
      on_response_received
        FOR EVENT response_received OF zcl_abap_http_client
        IMPORTING sender client.
ENDCLASS.

CLASS zcl_http_client_handler IMPLEMENTATION.
  METHOD on_before_request_sent.
    " Actions before request is sent (e.g., logging)
    WRITE: / 'Request is about to be sent'.
  ENDMETHOD.

  METHOD on_after_request_sent.
    " Actions after request is sent (e.g., logging)
    WRITE: / 'Request has been sent'.
  ENDMETHOD.

  METHOD on_response_received.
    " Actions when response is received (e.g., processing response)
    WRITE: / 'Response received'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  " Create a handler instance
  DATA(lo_handler) = NEW zcl_http_client_handler( ).
  
  " Create an HTTP client instance
  DATA(lo_client) = zcl_abap_http_client=>create(
    baseurl = 'https://example.com'
    path = '/data'
  ).

  " Set event handlers
  SET HANDLER lo_handler->on_before_request_sent FOR lo_client.
  SET HANDLER lo_handler->on_after_request_sent FOR lo_client.
  SET HANDLER lo_handler->on_response_received FOR lo_client.

  " Perform a GET request and send it
  lo_client->get(
  )->send(
    IMPORTING
      eo_response = DATA(lo_response)
    EXCEPTIONS
      http_communication_failure = 1
      OTHERS = 2
  ).
  IF sy-subrc <> 0.
    WRITE: / 'Request failed'.
    RETURN.
  ENDIF.

  cl_demo_output=>display_json( json = lo_response->get_cdata( ) ).
```

### GET Request Example
Example of making a GET request to fetch data from a URL:
```abap
START-OF-SELECTION.
  DATA(lo_client) = zcl_abap_http_client=>create(
    baseurl = 'https://api.example.com'
    path = '/data'
  ).

  lo_client->get(
    EXPORTING
      query_parameters = VALUE #( ( name = 'param1' value = 'value1' ) ) "?param1=value1
  )->send(
    IMPORTING
      eo_response = DATA(lo_response)
    EXCEPTIONS
      http_communication_failure = 1
      OTHERS = 2
  ).
  IF sy-subrc <> 0.
    WRITE: / 'GET request failed'.
    RETURN.
  ENDIF.

  cl_demo_output=>display_json( json = lo_response->get_cdata( ) ).
```

### POST Request with JSON Data
Example of making a POST request and sending data in JSON format:
```abap
START-OF-SELECTION.
  DATA(lo_client) = zcl_abap_http_client=>create(
    baseurl = 'https://api.example.com'
    path = '/submit'
  ).

  lo_client->post(
    EXPORTING
      body = '{ "key": "value" }'
      content_type = zcl_abap_http_client=>content_types-json
  )->send(
    IMPORTING
      eo_response = DATA(lo_response)
    EXCEPTIONS
      http_communication_failure = 1
      OTHERS = 2
  ).
  IF sy-subrc <> 0.
    WRITE: / 'POST request failed'.
    RETURN.
  ENDIF.

  cl_demo_output=>display_json( json = lo_response->get_cdata( ) ).
```

### PUT Request with Form Data
Example of making a PUT request and sending form data:
```abap
START-OF-SELECTION.
  DATA(lo_client) = zcl_abap_http_client=>create(
    baseurl = 'https://api.example.com'
    path = '/update'
  ).

  lo_client->put(
    EXPORTING
      form_fields = VALUE #( ( name = 'key1' value = 'value1' ) ( name = 'key2' value = 'value2' ) )
      content_type = zcl_abap_http_client=>content_types-form_urlencoded
  )->send(
    IMPORTING
      eo_response = DATA(lo_response)
    EXCEPTIONS
      http_communication_failure = 1
      OTHERS = 2
  ).
  IF sy-subrc <> 0.
    WRITE: / 'PUT request failed'.
    RETURN.
  ENDIF.

  cl_demo_output=>display_json( json = lo_response->get_cdata( ) ).
```

### DELETE Request Example
Example of making a DELETE request to remove a resource:
```abap
START-OF-SELECTION.
  DATA(lo_client) = zcl_abap_http_client=>create(
    baseurl = 'https://api.example.com'
    path = '/delete'
  ).

  lo_client->delete(
    EXPORTING
      query_parameters = VALUE #( ( name = 'id' value = '123' ) ) "?id=123
  )->send(
    IMPORTING
      eo_response = DATA(lo_response)
    EXCEPTIONS
      http_communication_failure = 1
      OTHERS = 2
  ).
  IF sy-subrc <> 0.
    WRITE: / 'DELETE request failed'.
    RETURN.
  ENDIF.

  cl_demo_output=>display_json( json = lo_response->get_cdata( ) ).
```

### Multipart File Upload
Example of making a POST request to upload a file using multipart data:
```abap
START-OF-SELECTION.
  DATA(lo_client) = zcl_abap_http_client=>create(
    baseurl = 'https://api.example.com'
    path = '/upload'
  ).

  DATA(lx_file_data) = zcl_example_reader=>read_file( path = 'path/to/your/file.txt' ). " type xstring
  
  lo_client->post(
    EXPORTING
      multipart_files = VALUE #( ( name = 'file' filename = 'file.txt' content_type = 'text/plain' data = lx_file_data ) )
      content_type = zcl_abap_http_client=>content_types-formdata
  )->send(
    IMPORTING
      eo_response = DATA(lo_response)
    EXCEPTIONS
      http_communication_failure = 1
      OTHERS = 2
  ).
  IF sy-subrc <> 0.
    WRITE: / 'POST request failed'.
    RETURN.
  ENDIF.

  cl_demo_output=>display_json( json = lo_response->get_cdata( ) ).
```

### POST Request with Authorization Header
Example of making a POST request with authorization by including an authorization header:
```abap
START-OF-SELECTION.
  DATA(lo_client) = zcl_abap_http_client=>create(
    baseurl = 'https://example.com'
    path = '/data'
  ).

  " Prepare authorization header
  DATA(lv_username_and_password) = |{ 'your_username' }:{ 'your_password' }|.

  DATA(lv_basic_auth) = |Basic { cl_http_utility=>encode_base64( unencoded = lv_username_and_password ) }|.

  lo_client->post(
    EXPORTING
      body = '{ "data":"123" }'
      header_fields = VALUE #( ( name = 'Authorization' value = lv_basic_auth ) )
  )->send(
    IMPORTING
      eo_response = DATA(lo_response)
    EXCEPTIONS
      http_communication_failure = 1
      OTHERS = 2
  ).
  IF sy-subrc <> 0.
    WRITE: / 'POST request failed'.
    RETURN.
  ENDIF.

  cl_demo_output=>display_json( json = lo_response->get_cdata( ) ).
```

Feel free to adjust any details according to your specific needs or preferences.
