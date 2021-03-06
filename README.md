easy-update-solr4files-index
===========
[![Build Status](https://travis-ci.org/DANS-KNAW/easy-update-solr4files-index.png?branch=master)](https://travis-ci.org/DANS-KNAW/easy-update-solr4files-index)


SYNOPSIS
--------

    easy-update-solr4files-index update [-s <bag-store>] <uuid>
    easy-update-solr4files-index init <bag-store>
    easy-update-solr4files-index run-service
    easy-update-solr4files-index delete <solr-query>
    
    Some examples of solr queries:
    
      everything:            '*:*'
      all bags of one store: 'easy_dataset_store_id:pdbs'
      a bag:                 'easy_dataset_id:ef425828-e4ae-4d58-bf6a-c89cd46df61c'
      a folder in a bag:     'id:ef425828-e4ae-4d58-bf6a-c89cd46df61c/data/files/Documents/*'


### HTTP service

When started with the sub-command `run-service` a REST API becomes available summarized in the following table.
"Method" refers to the HTTP method used in the request. "Path" is the path pattern used. 
Placeholders for variables start with a colon, optional parts are enclosed in square brackets.

Method   | Path                             | Args |Action
---------|----------------------------------|------|------------------------------------
`GET`    | `/fileindex`                     |      | Return a simple message to indicate that the service is up: "EASY File index is running."
`POST`   | `/fileindex/init[/:store]`       |      | Index all bag stores or just one. Eventual obsolete items are cleared.
`POST`   | `/fileindex/update/:store/:uuid` |      | Index all files of one bag. Eventual obsolete file items are cleared.
`DELETE` | `/fileindex/:store[/:uuid]`      |      | Remove all items or the items of a store or bag.
`DELETE` | `/fileindex/`                    | q    | Remove the items matching the mandatory solr query.

The following example would delete a bag

    http://easy.dans.knaw.nl/fileindex/?q=easy_dataset_id:ef425828-e4ae-4d58-bf6a-c89cd46df61c

DESCRIPTION
-----------

Update the EASY SOLR for Files Index with file data from a bag-store.

File content is indexed together with some file metadata as well as dataset metadata.
Files are only indexed if `easy_file_accessible_to` gets a value 
`anonymous`, `known`, `restrictedGroup` or `restrictedRequest`.
If the value is not provided at file level by `metadata/files.xml`,
a default is derived from `<ddm:profile><ddm:accessRights>` in `metadata/dataset.xml`.


ARGUMENTS
---------

    Options:

      --help      Show help message
      --version   Show version of this program

    Subcommand: update - Update accessible files of a bag in the SOLR index
      -s, --bag-store  <arg>   Name of the bag store (default = pdbs)
          --help               Show help message
    
     trailing arguments:
      bag-uuid (required)
    ---
    
    Subcommand: delete - Delete documents from the SOLR index
          --help   Show help message
    
     trailing arguments:
      solr-query (required)
    ---
    
    Subcommand: init - Rebuild the SOLR index from scratch for active bags in one or all store(s)
          --help   Show help message
    
     trailing arguments:
      bag-store (not required)
    ---
    
    Subcommand: run-service - Starts EASY Update Solr4files Index as a daemon that services HTTP requests
          --help   Show help message
    ---

EXAMPLES
--------

    easy-update-solr4files-index update 9da0541a-d2c8-432e-8129-979a9830b427


INSTALLATION AND CONFIGURATION
------------------------------


1. Unzip the tarball to a directory of your choice, typically `/usr/local/`
2. A new directory called easy-update-solr4files-index-<version> will be created
3. Add the command script to your `PATH` environment variable by creating a symbolic link to it from a directory that is
   on the path, e.g. 
   
        ln -s /usr/local/easy-update-solr4files-index-<version>/bin/easy-update-solr4files-index /usr/bin



General configuration settings can be set in `cfg/application.properties` and logging can be configured
in `cfg/logback.xml`. The available settings are explained in comments in aforementioned files.


BUILDING FROM SOURCE
--------------------

Prerequisites:

* Java 8 or higher
* Maven 3.3.3 or higher

Steps:

        git clone https://github.com/DANS-KNAW/easy-update-solr4files-index.git
        cd easy-update-solr4files-index
        mvn install
