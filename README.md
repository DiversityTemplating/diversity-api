# diversity-api

An erlang diversity api that serves files and settings on the fly.

## Requirements

Diversity-api requires Erlang/OTP R17 which can be downloaded here:  
https://www.erlang-solutions.com/downloads/download-erlang-otp

## Configuration

Before compiling you need to configure where the git repos should be cloned to and
what port to use for the api server. This is currently done in the `rel/sys.config` file:

- ``` repo_dir ``` Where all diversity components will be checked out to.
- ``` nodes ``` List of nodes where updates and registers will be called on. Empty list if no other nodes exists.
- ``` port ``` Which port the node will be listening to.

```Erlang
[{
    divapi,  [{repo_dir, "path/to/your/directory"},
              {nodes, []}
              {port, 8181}]
}].
```
### Note:
To configure or change configuration after building the release, the sys.config used by the release are located in
`_rel/divapi_release/releases/1/sys.config`.

## Building
`make` - Fetches dependencies, compiles and builds a release in _rel/divapi_release/

## Running
- `./_rel/divapi_release/bin/divapi_release start` - start the server
- `./_rel/divapi_release/bin/divapi_release stop`  - stop the server
- `./_rel/divapi_release/bin/divapi_release console`  - start the server with a console

## Usage


#### List all available components
```
GET /components/

Response:
[{"version":"0.0.4","title":"My First Component,"name":"my-component1","description":"A component that will do stuff"},
 {"version":"1.0.0","title":"My Second Component","name":"my-component2","description":"A component that will do even more stuff"}]
```
#### Lists all tagged versions of the component
```
GET /components/my-component1/

Response:
["0.0.1", "0.0.2", "0.0.3", "0.0.4"]
```

#### Lists all components that have sidebar configured in their grouping attribute.
```
GET /components/?grouping=sidebar

Response:
["my-component2"]
```
#### Get diversity.json from component with version 1.0.0
```
GET /components/my-component2/1.0.0/

Response:
{
  "name": "my-component2",
  "title": "My Second Component",
  "description": "A component that will do even more stuff",
  "version": "1.0.0",
  "angular": "myComponent2",
  "template": "component.mustache.html",
  "script": [
    "scripts.min.js"
  ],
  "settings": {
    "type": "object",
    "properties": {
      "classes": {
        "type": "array",
        "items": {
          "type": "string"
        }
      }
    }
  },
  "dependencies": {
    "my-component1": "^0.0.1",
  }
}
```
#### Get diversity.json from latest patch-version of component 0.1.*
```
GET /components/my-component2/0.1/
```
#### Get diversity.json from latest version of the component
```
GET /components/my-component2/*/
```
#### All the component's CSS and SCSS compiled with the variables passed in via the query string
```
GET /components/my-component2/1.0.0/css?foo=bar

Response: A CSS file
```
#### Get the settings attribute from the diversity.json from component with version 1.0.0
```
GET /components/my-components2/1.0.0/settings

Response:
{
  "settings": {
    "type": "object",
    "properties": {
      "classes": {
        "type": "array",
        "items": {
          "type": "string"
        }
      }
    }
  }
}
```
#### Get the settingsForm attribute from the diversity.json from component with version 1.2.3
```
GET /components/my-component/0.0.1/settingsForm

Response:
{}
```
#### Get thumbnail picture from component with version 1.2.3
```
GET /components/<component>/1.2.3/thumbnail

Response: Image data
```
#### Fetch file.js from component with version 1.0.0
```
GET /components/my-component2/1.0.0/files/script.min.js

Response: The requested files from the component
```

#### Note:
All the version selection methods can be used to get settings, settingsForm and files from the selected version.


## To register a component
```sh
$ curl -X POST -d repo_url=https://github.com/foobar/foo.git example.com/components/foo/register
```
## Updating a component

```sh
$ curl -X POST example.com/components/foo/update
```
