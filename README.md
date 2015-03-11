diversity-api
=============

An erlang diversity api server

Requirements
------------

Diversity-api requires Erlang/OTP R17 which can be downloaded here:
https://www.erlang-solutions.com/downloads/download-erlang-otp

Configuration
-------------

Before compiling you need to configure where the git repos should be cloned to and
what port to use for the api server. This is currently done in the `rel/sys.config` file:

``` rel/sys.config
[{
    divapi,  [{repo_dir, "path/to/your/directory"},
              {port, 8181}]
}].
```

To configure or change configuration after building the release, the sys.config used by the release are located in
`_rel/divapi_release/releases/1/sys.config`.

Building
--------

`make` - Fetches dependencies, compiles and builds a release in _rel/divapi_release/

Running
-------

`./_rel/divapi_release/bin/divapi_release start` - start the server
`./_rel/divapi_release/bin/divapi_release stop`  - stop the server


Usage/Routes
------------

GET

`/components/<component>/` - Lists all tagged versions of the component
`/components/` - Lists information of all public components on diversity.io
`/components/?grouping=sidebar` - Lists all components that have sidebar configured in their grouping attribute.

`/components/<component>/1.2.3/` - diversity.json from component with version 1.2.3
`/components/<component>/1.2/` - diversity.json from latest patch-version of component 1.2.*
`/components/<component>/*/` - diversity.json from latest version of the component

`/components/<component>/1.2.3/css?foo=bar` - All the component's CSS and SCSS compiled with the variables passed in via the query string
`/components/<component>/1.2.3/settings` - settings attribute from the diversity.json from component with version 1.2.3
`/components/<component>/1.2.3/settingsForm` - settingsForm attribute from the diversity.json from component with version 1.2.3
`/components/<component>/1.2.3/thumbnail` - thumbnail picutre from component with version 1.2.3
`/components/<component>/1.2.3/files/path/to/file.js` - file.js from component with version 1.2.3

Note: All the version selection methods can be used to get settings, settingsForm and files from the selected version.


To register a component
-----------------------

POST with parameter repo_url=url_to_repo
`/components/<componentname>/register/`

Ex with curl.
```sh
$ curl -d repo_url=https://git.diversity.io/foobar/foo.git api.diversity.io/components/foo/register
```

Updating a component
-----------------------

`/component/<componentname>/update`
