# Dropbox for LispWorks

The `dropbox` package is a wrapper for the [CORE Dropbox REST API](https://www.dropbox.com/developers/core/docs). It uses [OAuth 2.0](http://oauth.net/) for authentication and allows for manipulation of Dropbox data.

This package requires the [`http`](https://www.github.com/massung/http) and [`json`](https://www.github.com/massung/json) packages (each has their own dependencies as well). Many of the Dropbox functions return RFC822 dates, which can be parsed into universal times with the [`date`](https://www.github.com/massung/date) package, but it isn't strictly required.

## Quickstart

There are special variables you'll need to set before you can use this tutorial: `*app-key*`, `*app-secret*`, and `*app-root*`.

	(setq dropbox:*app-key* "myappkey")
	(setq dropbox:*app-secret* "myappsecret")
	(setq dropbox:*app-root* :sandbox)

The variable `*app-root*` needs to be set to either `:dropbox` or `:sandbox`, based on the type of application you have (either full access or limited to just the `/Apps` folder). The default value is `:sandbox`.

First, you'll need to request a token.

	CL-USER > (dropbox-request-token)
	#<HQN-WEB::INTERNET-EXPLORER HINSTANCE 42>

This will open up a web browser window to Dropbox. If the user is already logged in it will prompt them to allow your application access. Otherwise it gives them the ability to log in first and then allow access.

Once access has been granted, a token will be displayed on the webpage, which must then be used to complete the process.

	CL-USER > (setf tok (dropbox-request-access token))
	#<DROPBOX::ACCESS-TOKEN 200A5CFF>

Now that you have the token, you can use it to perform all sorts of fun Dropbox actions.

	CL-USER > (dropbox-account-info tok)
	#<DROPBOX::ACCOUNT-INFO 200CB5D3>

How about manipulating a file...

	CL-USER > (dropbox-files-put tok "hello.txt" "Hello, world!")
	#<DROPBOX::METADATA 21DD1A8B>

	CL-USER > (dropbox-files-get tok "hello.txt")
	"Hello, world!"
	#<DROPBOX::METADATA 200C765F>

	CL-USER > (dropbox-files-put tok "hello.txt" "Cya later!")
	#<DROPBOX::METADATA 21E04E17>

	CL-USER > (dropbox-revisions tok "hello.txt")
	#(#<DROPBOX::METADATA 20091793> #<DROPBOX::METADATA 20091747>)

	CL-USER > (metadata-rev (elt * 1))
	"3f00ffe33c8"

	CL-USER > (dropbox-restore tok "hello.txt" *)
	#<DROPBOX::METADATA 21DF16CF>

	CL-USER > (dropbox-delete tok "hello.txt")
	#<DROPBOX::METADATA 21DEC3CF>

While performing the above round of functions to try things out, if you had Dropbox installed on your local machine you should have been seeing the file get sync'd and updated as you performed each operation.

## API Documentation

#### Special Variables

	*app-key*
	*app-secret*
	*app-root*

#### Conditions

A `dropbox-error` is signaled whenever a call to a Dropbox end-point fails. This error contains the reason for the failure as well.

#### OAuth Authentication

	(dropbox-request-token)

This function will open a browser window and request authorization for your application (set in `*app-key*`) from the user. If they allow it, then it will redirect them to another page containing a token they should paste into your application...

	(dropbox-request-access token-string)

Called with the token provided by Dropbox and pasted into your application by the user. Returns an `access-token` object upon success, which is then used in all other Dropbox API calls.

#### Dropbox Core API

*NOTE: All optional parameters (keyword parameters) to Dropbox CORE end-points are the same names as found [here](https://www.dropbox.com/developers/core/docs) and should be the values expected by the API. For example, don't pass `:overwrite t` since Dropbox doesn't know how to interpret `"t"`. Instead pass `:overwrite "true"`.*

	(dropbox-account-info token)

Returns an `account-info` object containing ingormation about the Dropbox user (name, email, etc).

	(dropbox-copy token from to-path &key locale)

Copies a file or folder from a path or `copy-ref` object to another path.

	(dropbox-copy-ref token path)

Returns a `copy-ref` object to use with `dropbox-copy`.

	(dropbox-create-folder token path)

Recursively creates a folder. Returns the `metadata` object for the newly created folder.

	(dropbox-delete token path)

Deletes a file or folder. Returns the `metadata` object for the deleted file.

	(dropbox-files-get token path &key rev)

Returns both the contents of the file `metadata` object about the file as multiple values. Optionally can include a specific revision to get.

	(dropbox-files-put token path data &key parent_rev overwrite locale)

Writes data to the given path. Returns the `metadata` object for the new file.

	(dropbox-media token path &key locale)

Returns a `media` object that contains a url and expiration date (for the url).

	(dropbox-metadata token path &key file_limit hash list include_deleted rev locale)

Returns the `metadata` for a given file or path. If a path, then the `metadata-contents` slot contains an array of `metadata` objects, identifying the files in the path.

	(dropbox-move token from-path to-path &key locale)

Moves a file or folder from one location to another. Returns the `metadata` object for the newly created file.

	(dropbox-restore token path rev &key locale)

Restores a file to a previous revision (obtained via the `metadata-rev` slot in a `metadata` object).

	(dropbox-revisions token path &key rev_limit locale)

Returns an array of `metadata` objects for all the revisions for a given file or path.

	(dropbox-search token path query &key file_limit include_deleted locale)

Returns an array of `metadata` entires for any filename or folder that matches the query string.

	(dropbox-shares token path &key short_url locale)

Returns a `media` object with a url and expiration date for the url.

	(dropbox-thumbnails token path &key format size)

Returns the binary data for the thumbnail of a file (if one exists for the file) and the `metadata` for the file.

