## CODA Taxi Times

David Marsh 
Created: 28 September 2018

Eurocontrol/CODA has been publishing taxi times at airports for some years, at www.eurocontrol.int/coda.
These are calculated from flight-by-flight data provided to CODA by airlines and airports.
Airports are included where CODA receives data on more than 100 flights, so it covers airports small and large, mostly in Europe, but some non-European ones with direct flights to Europe.

After each IATA season (Summer = end March-end October, Winter), 3 reports are published: taxi-in times, taxi-out times, and taxi-out split by ICAO wake turbulence category (WTC). The files were originally pdf, now xlsx.

This project was for me a learning exercise in web-scraping, and in using git and github.

The code merges all the published files into a single, tidy dataset.

However, the pdf reports by WTC were difficult to read because of the row alignment in the tables, so these are not currently included.
Perhaps `tabulizer` would handle this, but I gave up after wasting some hours trying to get its java to work on my machine, and parsed the tables manually from the text.

## Visualisation

The data are also provided in a Tableau public visualisation at: https://public.tableau.com/views/AirportTaxiTimes_0/HowLong? .
