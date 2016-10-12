# unitncal

**calendar scraper for unitn.it, live at http://unitncal.fgaz.me**

This script downloads the calendar files used by unitn's "orari" webapp and
converts them from the custom format to standard iCal, which is then
easily imported in many calendar apps.

## Usage

Simply run the script using python 2.
No additional libraries are required.

The ical files will be saved in a directory called "cal".

## Deployment

This script can be easily deployed to a paas.

In openshift, for example:

* add an application using the php cartridge (you don't need php, but the cartridge contains a python interpreter and a static file server)
* add the cron cartridge
* add the script to the .openshift/cron/hourly directory and to the start hooks directory
* create an index file
