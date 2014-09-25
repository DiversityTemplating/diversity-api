diversity-api
=============

An erlang diversity api server

Configuration
-------------

Before compiling you need to configure where the git repos should be cloned to, the credentials to diversity.io and
what port to use for the api server. This is currently done in the `rel/sys.config` file:

``` rel/sys.config
[{
    divapi,  [{repo_dir, "path/to/your/directory"},
              {token, "s3cr3tt0k3n"},
              {port, 8181}]
}].
```

The token can be obtained in your Account settings page on diversity.io (`Profile settings -> Account`)

Building
--------

`make` - Compiles and builds a release in _rel/divapi_release/

Running
-------

`./_rel/divapi_release/bin/divapi_release start` - start the server
`./_rel/divapi_release/bin/divapi_release stop`  - stop the server


Usage/Routes
------------

`/components/<component>/` - Lists all tagged versions of the component
`/components/` - Lists information of all public components on diversity.io
`/components/?grouping=sidebar` - Lists all components that have sidebar configured in their grouping attribute.

`/components/<component>/1.2.3/` - diversity.json from component with version 1.2.3
`/components/<component>/1.2/` - diversity.json from latest patch-version of component 1.2.*
`/components/<component>/*/` - diversity.json from latest version of the component

`/components/<component>/1.2.3/settings` - settings attribute from the diversity.json from component with version 1.2.3
`/components/<component>/1.2.3/settingsForm` - settingsForm attribute from the diversity.json from component with version 1.2.3
`/components/<component>/1.2.3/files/path/to/file.js` - Gets the file file.js

Note: All the version selection methods can be used to get settings, settingsForm and files from the selected version.


