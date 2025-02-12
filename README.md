Introduction
=====

What's Humble Bundle?
-----
It's an online shop where you can buy bundles of digital goods, like video games, eBooks, audiobooks, and comics, for a price that you set yourself! What you pay is split into three portions, one going towards charity, one to pay developers or authors, and one as a tip for Humble Bundle itself. You can even set rations between those portions, how much of your money each party receives! More details: https://www.humblebundle.com/

What's a Humble Bundle downloader?
-----
Some bundles are large! I mean, laaarge! If a bundle contains 15 games in versions for Windows, Linux and Mac, it's already 45 files. And very often there are additional patches, soundtracks, extra packs and bonuses. It's not unusual for a comics bundle to contain over 100 files you can download.

Now, you probably don't need all those files, you can just cherry-pick the platform and the format you are most interested in and download just them. If that's the case then you probably don't need a 'downloader' :-)

For others who like to have files they bought on their disk, be it for easier access or as a backup, a 'downloader' is a piece of software that automates the downloading of those files.

What's Humbundee?
-----
Yes, it's a Humble Bundle downloader. It's an application written in Erlang that given a Humble Bundle key downloads files contained in that bundle. But not only. It remembers MD5/SHA1 checksums of all downloaded files so that it doesn't download files that have been downloaded previously. Oh yes, and also allows to define regex patterns to skip particular files from the download if, for example, you are not interested in 32-bit Linux packages or games with German user interface.

Why Humbundee?
-----
Hmmm. You mean, why I wrote it or why you should use it? :-) Not that there is a fierce competition, I just couldn't find anything that would remotely have the features I needed. Then I thought that I will spend less time writing it than downloading all the files in all the bundles that I have bought so far and will buy in the future.

But let's see what Humbundee is capable of:

### Download Method

Humbundee downloads files using `wget`, ensuring that files are retrieved directly from their source. 

### Verifies checksums and sizes
All downloaded files are checked if they are of the expected size and if their MD5/SHA1 checksums match those advertised on the bundle page (actually, SHA1 checksums are not advertised but they are available in the metadata describing the bundle).

### Writes useful logs
Humbundee maintains a comprehensive log per each downloaded bundle. Not only it logs all files and torrents downloaded in the bundle, but also records all encountered problems, and at the end adds a single line of the download summary. See for yourself:

    2016-02-06 20:29:45.731145> Finished all downloads, Expected: 45, Downloaded: 30, Ignored: 0, Excluded: 15, Errors: 0, Status: ok, closing log.

### Excludes unwanted files
You may not want to download installation packages for Mac OS X or Linux 32-bit if you don't own those platforms. Or you may not speak German and don't want games featuring that language. Humbundee allows you to define a list of regex patterns to exclude downloads if the pattern is present in a name, URL, or platform associated with a particular download.

### Ignores already downloaded files
Sometimes the same game is included in more than one bundle. You are smart enough to not download again games you already downloaded from other bundles (if you remember them). A not-so-smart downloader may try to download everything you throw at it. But not Humbundee. It remembers all downloaded files, not by their names but sizes and MD5/SHA1 checksums, so it knows when a file has been downloaded previously even if it has a different name.

### Neatly organizes all downloads
If a bundle contains a few dozens of files you don't want to end up with all of them in a single folder. Humbundee automatically organizes all downloaded files into a hierarchy of folders under the main folder named after the bundle key. See the configuration section below for more details.

-----

Briefing
=====

What's needed?
-----

### Any Unix-like system
By this I mean that at this moment Humbundee probably won't work on Windows, and certainly it won't compile on Windows. It may work in a Unix-like environment on Windows (cygwin, MSys2) but that hasn't been tested.

### Erlang
Installed in version that supports `maps`. I developed Humbundee with Erlang 18 but 17 should be fine (albeit I didn't check that). Check if it's installed and in which version with:

    erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

### GNU make
It's a standard on Linux and available to be installed on other platforms. On *BSD check with:

    which gmake

### git
It's needed to clone the repository to your disk and also to fetch the build scripts (done automatically). At least version `1.6.x` would be needed.

### libyajl
Libyajl headers need to be installed on your system, used to parse JSON.

### Libyajl Installation  

To install Libyajl on your system, follow the appropriate steps for your operating system:  

#### Ubuntu/Debian  
```bash
sudo apt update
```
``` bash
sudo apt install libyajl-dev 
```
```bash Fedora  
~sudo dnf install yajl-devel
```  

#### Arch Linux  
```bash
sudo pacman -S yajl
```  

#### macOS (using Homebrew)  
```bash
brew install yajl
```  

#### FreeBSD  
```sh
pkg install yajl
``` 

After installing, ensure the headers (`yajl/yajl_parse.`) are available in your system's include path.


### wget
Also widely available to be installed from a package. Check if it's installed and available in the path with:

    wget --help

### cookie
You need to be logged in to Humble Bundle in order to download files from your bundle. But currently Humbundee doesn't allow you to log in. Instead, it expects you to log in using a normal browser and export a cookie which it can then use use to access the part of the website that only you are authorized to use. The cookie must be in a format compatible with `wget`. I use [this extension](https://chromewebstore.google.com/detail/get-cookiestxt-locally/cclelndahbckbenkjhflpdbgdldlbecc) to export the cookie from Chrome.

How it works?
-----

When you ask it to download a bundle, Humbundee downloads a `JSON` file that describes the bundle and starts a new Erlang `gen_server` called `hbd_id` to process the file. Humbundee can download multiple bundles simultaneously. Each `hbd_id` spawns one Erlang process per bundle position, which is usually a game, eBook or soundtrack. Then each of those processes spawns one new Erlang process per file that can be downloaded. There are usually multiple files per bundle position, e.g. Linux, Windows and Mac versions of a game, or EPUB, PDF and CBZ versions of an eBook. 

So, at this moment we have one Erlang process per each file that can be downloaded. For simplicity lets focus on what a single process does.

Firstly, it checks if the file can be downloaded at all. Some files, like streams or embedded JavaScript applications can't be downloaded. And so it dies if the file can't be downloaded. Secondly, it checks if the file has already been downloaded, and it dies if it has. Thirdly, it checks if the file should be excluded, i.e. if any of the regex patterns match, and it dies if yes. 

If all is good the process tries to download the torrent file with `wget`. If the torrent file doesn't exist, or can't be downloaded, or is incorrect, or can't be parsed, then the process discards information about the torrent file and the file will be downloaded as if the torrent didn't exist.

Please note that up to this point, all processing has been done in parallel, including downloading files with `wget`. While downloading many small files simultaneously isn’t a big issue, handling multiple large downloads at once can be resource-intensive. To manage this efficiently, the actual downloading is handled by a separate `gen_server` called `hbd_pool`, which coordinates `wget` downloads to ensure controlled and efficient file retrieval.  


Each process that hasn't died so far adds its download to `hbd_pool` and waits until the download is completed. The download isn't started immediatelly but `hbd_pool` adds them to a queue and starts when the amount of active downloads is less than the specified threshold. Active downloads are started with `wget`.

All downloads are handled by `wget`, which saves files directly into the `temp` folder while they are in progress. The `temp` folder is also used by `hbd_pool` to store temporary versions of files being downloaded. Once a download is complete, the file is moved to the `downloads` folder.  

Every two seconds, `hbd_pool` performs three important checks:

 1. List completed downloads in the `downloads` folder. Every completed download is moved to the destination folder constructed from the download properties, i.e.: `<key>/<publisher>/<title> - <platform>`. 
 2. Start new downloads from the queue up to the maximum allowed amount of active downloads.
 3. List pending downloads in the `temp` folder if the map with active downloads isn't yet emptied. The `temp` folder will be empty when all `wget` downloads have finished. If some files remain in the active downloads list but haven't progressed for more than 10 seconds, `hbd_pool` considers them stale and restarts the download using `wget`.  


Let's reiterate the last point. When the `temp` folder is empty but the map containing all active downloads isn't, it means that some files have been added but are not being actively downloaded. If those files have been in the list for more than 10 seconds without progress, they are considered stale and will be reattempted using `wget`.  

Each download always includes a direct HTTP/S URL, ensuring that `wget` can retrieve the file reliably. If a file isn't progressing, removing the temporary file from the `temp` folder will trigger `hbd_pool` to detect it as stale and restart the download using `wget`.  

Now lets come back to the processes started by `hbd_id` which are still waiting for the downloads to be completed. For each file that `hbd_pool` moves from the `downloads` folder to the destination folder it also sends back a response to the waiting process which initially added the file to `hbd_pool`. Each `hbd_id` remembers how many processes have been started to download files in the given bundle. The bundle download is finished when all those processes die after receiving either an OK that the file has been downloaded properly or an error. When no child process is left `hbd_id` adds a summary to the log file, then the log file is closed and the `hbd_id` dies as well.

Short note about indexing
-----
Each file contained in a bundle and processed by Humbundee is added to an Index. Index is simply a mnesia table containing [`idx` records](https://github.com/amiramix/humbundee/blob/master/lib/humbundee/include/humbundee.hrl#L25). The table is indexed by the bundle key and MD5 and SHA1 checksums of the file. Additionally it stores the file size and available JSON metadata of the downloaded file. You can query entries in the table with `humbundee:read/1` and delete with `humbundee:delete/1`.

All downloaded files are automatically added to the index. However, it's also possible to index files contained in a bundle without downloading them. This will be useful if you have already downloaded a bundle manually but still want to add it to the index so that Humbundee doesn't download its files again in case they are also present in another bundle. For that you can use `humbundee:index/1` (see the Available commands section below).

This can be also used for testing purposes, i.e. to verify that exclude patterns work as expected. For that you could employ the following steps:

 1. Configure Humbundee (add desired exclude patterns, configure folders, etc).
 2. Start Humbundee
 3. Index the bundle with `humbundee:index/1`
 4. Check download logs for any errors and also to verify if the desired files have been excluded
 5. Delete the download folder from the `destination_dir` folder
 6. Delete the bundle from the index with `humbundee:delete/1` and repeat

Please note that `hbd_idx` only cares about duplicate files and is not trying to index all downloaded files. Therefore, if the same file with the same MD5/SHA1 checksum is contained in multiple bundles with possibly different names, titles, publishers and bundle Id's, only one version of that file will be stored in the index with one specific set of metadata information.

-----

All about configuration
=====

Configure Humbundee
-----

The main, default configuration is in the [humbundee/etc/sys.config.src](https://github.com/amiramix/humbundee/blob/master/etc/sys.config.src) file.

When Humbundee starts it additionally tries to read file `.humbundee.conf` from the user's home directory, i.e.: `~/.humbundee.conf`. Then for each option found in the user configuration file it replaces the default value with the user defined one. In other words, user options take precedence but if they are not defined then the defaults are used.

### Example of a user configuration file:

    %% -*- erlang -*-
    {cookie,          <<"/home/user/Downloads/cookies.txt">>}.
    {download_dir,    <<"/home/user/archive/downloads">>}.
    {temp_dir,        <<"/home/user/archive/temp">>}.
    {destination_dir, <<"/home/user/archive/humblebundle">>}.
    {exclude_regex_list,
     [
      {name, <<"^MOBI$">>, [caseless]},
      {name, <<"^32-bit">>},
      {name, <<"32-bit$">>},
      {name, <<".rpm$">>},
      {name, <<"^720p$">>},
      {name, <<"^German$">>, [caseless]},
      {platform, <<"ebook">>}
     ]}.
    {workers, 20}.

The meaning of these options is as follows:

#### `cookie`
Location of the cookie file

#### `download_dir`
Folder where completed downloads are stored. Humbundee monitors this folder and moves completed files to an appropriate subfolder in the `destination_dir`.

#### `temp_dir`
Folder where `wget` store incomplete downloads.

#### `workers`
The maximum number of concurrent downloads that can be active at the same time is limited by this threshold. The sum of downloads that `hbd_pool` starts with `wget` cannot exceed this limit.  

#### `destination_dir`
A parent folder for completed bundles. All files in completed bundles are stored in subfolders named according to the following pattern:

    Key
      .etr, .json, .log
      *_data
        *- Publisher
          *- Title - platform

Where:

 * `Key` is the bundle key, an alphanumeric set of characters associated with each bundle.
 * `.etr`, `.json`, `.log` are files produced when downloading the bundle.
 * `_data` is the parent folder for all downloaded files.
 * `Publisher` is usually the company behind the digital goods, games, comics, etc.
 * `Title` is the title of the digital item, a game title, a book title, etc.
 * `Platform` is one of android, linux, windows, comics, comedy, ebook, audio, etc.

#### `exclude_regex_list`

List of tuples `{Match, Pattern}` or `{Match, Pattern, Options}` where:

 * `Match` - one of `name`, `link`, `platform` and `any`.
 * `Pattern` - the `Regexp` variable accepted by [re:compile/2](http://erlang.org/doc/man/re.html#compile-2)
 * `Options` - list of options accepted by either [re:compile/2](http://erlang.org/doc/man/re.html#compile-2) or [re:run/4](http://erlang.org/doc/man/re.html#run-3)

Humbundee tries to match `Pattern` against the content of specific fields in the JSON file according to `Match` and excludes the file from the download if the match is successful. The following fields in the Humble Bundle JSON file are matched when the `Match` is:

 * `name` - fields: folder (payee, usually the publisher), title (human_name, usually the position name on the bundle page), machname (a shortened concatenation of the title and platform), name (text on the download button)
 * `link` - fields: web and bittorrent urls
 * `platform` - field: platform
 * `any` - all above, file is excluded if any of those fields match

The `Options` list may contain options accepted by both, [re:compile/2](http://erlang.org/doc/man/re.html#compile-2) and [re:run/4](http://erlang.org/doc/man/re.html#run-3). Humbundee uses those accepted by [re:compile/2](http://erlang.org/doc/man/re.html#compile-2) when compiling the regex and filters them out so that when [re:run/4](http://erlang.org/doc/man/re.html#run-3) is later called to check the match only options accepted by [re:run/4](http://erlang.org/doc/man/re.html#run-3) are passed to it.



### Configure the `download_dir` folder

On the **Downloads** tab edit _Save files to location:_. It must have the same value as `download_dir` configured for Humbundee in the previous section.

### Configure the `temp_dir` folder

On the same **Downloads** tab edit _Keep incomplete torrents in:_. It must have the same value as `temp_dir` configured for Humbundee in the previous section.



Other options, especially those on **Connection** and **Speed** tabs can be configured to match your network link speed. They don't affect Humbundee in any way.

--------------------------------------

The nitty-gritty - how to use Humbundee
=====

Let's get started!
-----

### Install prerequisites


Install Erlang, GNU make, git wget,yajl.

### Download the cookie

Log in to Humble Bundle and download the cookie in a format compatible with `wget`. [This extension](https://chrome.google.com/webstore/detail/cookiestxt/njabckikapfpffapmjgojcnbfjonfjfg) can be used to export the cookie from Chrome.

### Configure Humbundee

Instead of updating the default configuration [humbundee/etc/sys.config.src](https://github.com/amiramix/humbundee/blob/master/etc/sys.config.src) it's easier to create a new file `.humbundee.conf` in the user's home directory, i.e.: `vi ~/.humbundee.conf`.

See the section **All about configuration** above for details.



### Compile Humbundee

Note1: use `gmake` (GNU make) instead of `make` on *BSD systems.  
Note2: `make dev` must be issued after `make get-deps` has completed, `make get-deps dev` won't work.

    git clone https://github.com/amiramix/humbundee.git
    cd humbundee/
    make get-deps
    make dev

### Install Humbundee

    ./bin/init.esh

### Start Humbundee

    ./bin/start.esh

### Open the Erlang shell

    to_erl ../hbd/shell/

### Start and manage downloads

Humbundee is managed from Erlang shell by calling functions exposed in the [humbundee.erl](https://github.com/amiramix/humbundee/blob/master/lib/humbundee/src/humbundee.erl) module. See the next section for details.

### Stop Humbundee (and close the VM)

    q().

This is to be issued in the Erlang shell of course. Alternatively, in another console do in the Humbundee root folder:

    ./bin/stop.esh


Available commands
-----

The following commands can be issued in the Erlang shell to start and control downloads. All commands accept either strings or binaries.

### `humbundee:download/1`

    humbundee:download(Key) -> ok | {error, Error}

Types:

    Key = binary() | string()
    Error = any()

Starts downloading the bundle with the given key. The function spawns a new process to handle the download and returns immediately. You can start downloading another bundle at any time, all downloads are handled in parallel. Please use `humbundee:status/1` to get information about the status of downloading a particular bundle and `humbundee:status/0` to get information about the status of the download queue `hbd_pool`.

### `humbundee:index/1`

    humbundee:index(Key) -> ok | {error, Error}

Types:

    Key = binary() | string()
    Error = any()

Works almost exactly like `humbundee:download/1`, however each download process dies just before the file would be added to the download queue. The primary purpose of this function is to add a bundle to the index without actually downloading files contained in the bundle. This is useful if you have already downloaded a bundle but still want the files contained in the bundle to be indexed so that Humbundee doesn't try to download those files if they are distributed again in another bundle. Files indexed in a particular bundle can be queried using `humbundee:read/1` and deleted from the index using `humbundee:delete/1`.

### `humbundee:status/0`

    humbundee:status() -> [{downloading, Keys}, {bad, Bad}, {waiting, Waiting}, {wget, Pids}]

Returns status of the download queue in `hbd_pool`.

Where:

 * `Keys` - List of active downloads - files currently being downloaded.
 * `Bad` - List of files downloaded with an error. Those files are not moved to the destination folder but left in the `downloads` folder for further manual inspection.
 * `Waiting` - Amount of downloads waiting in the queue - files not yet started downloading.
 * `Pids` - List of processes currently downloading files with `wget`.

### `humbundee:status/1`

    humbundee:status(Key) -> [{id, Id}, {cookie, Cookie}, {out, Out}, {count, Count},
     {stats, {OK, Ignored, Excluded, Error}}, {pids, Pids}]

Types:

    Key = binary() | string()

Returns the status of downloading a bundle with the specified key.

Where:

 * `Id` - The Key of the bundle
 * `Cookie` - Location of the cookie file
 * `Out` - Root of the download directory for files in this bundle
 * `Count` - Amount of expected downloads for this bundle
 * `OK` - Amount of files downloaded successfully so far
 * `Ignored` - Amount of ignored files so far
 * `Excluded` - Amount of excluded files so far
 * `Error` - Amount of downloads completed with an error so far
 * `Pids` - Remaining processes that are either still processing downloads or which added their downloads to `hbd_pool` and are waiting for the downloads to be completed

### `humbundee:read/1`

    humbundee:read(Id) -> [#idx{}]

Types:

    Id = binary() | string()

Returns list of [idx records](https://github.com/amiramix/humbundee/blob/master/lib/humbundee/include/humbundee.hrl#L25) for which the provided Id matched one of the following: bundle key, file MD5 checksum, file SHA1 checksum.

The function automatically recognizes if the provided Id is a checksum or a bundle key and acts appropriately. For MD5/SHA1 it will usually return a list with one item - the file for which the MD5/SHA1 matches. Unless there is no such file, in which case it will return an empty list. For bundle keys it will return a list with all items belonging to that bundle.

### `humbundee:delete/1`

    humbundee:delete(Id) -> [Result]

Types:

    Id = binary() | string()

Works exactly like `humbundee:read/1` but instead of returning the read records deletes them and returns a list containing results of deleting those records - one result per deleted record.

Examples
-----

    humbundee:index(<<"U6tNd3hXZUT8uCg2">>).
    humbundee:delete("U6tNd3hXZUT8uCg2").
    humbundee:download("U6tNd3hXZUT8uCg2").
    humbundee:status(<<"U6tNd3hXZUT8uCg2">>).
    humbundee:status().
    humbundee:read("U6tNd3hXZUT8uCg2").
    humbundee:read("8b6f1882f93f3c5009900e09759fbcb3e8f04deb").
    humbundee:delete(<<"5029763c40d31842637a5e8f4165a245">>).

Where:

    U6tNd3hXZUT8uCg2 - bundle key
    8b6f1882f93f3c5009900e09759fbcb3e8f04deb - SHA1 checksum
    5029763c40d31842637a5e8f4165a245 - MD5 checksum

Troubleshooting
-----

Command `./bin/init.esh` installs Humbundee into the folder `../hbd/`. So, the folder containing Humbundee sources and `hbd` become siblings. The `hbd` folder is called **node root**. It contains files produced by the node when it starts and downloads bundles, and also will contain the downloaded files if you haven't changed the default configuration. One of the folders that will be useful for troubleshooting is `hbd/log`. Especially `yolf.log` will contain the starting configuration and any errors encountered when starting the node. Crashes will also be logged to `lager` logs. All messages printed by the Erlang VM during the booting process will be logged to `erlang.log.*` logs.
