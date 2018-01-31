This code creates visual timelines that display the time period continuous data loggers (or field meters) have been deployed by site. It presents four examples to familiarize the user with the package timevis(). The code can be modified in a number of ways, including showing the reverse, the sites associated with each logger. It could also be easily modified for other project management applications.

There are four examples in this script, each with more complexity (data formatting) required.  All dataframes are provided in the code.

1: Example from the timevis() help.

2: Timeline from a dataframe containing logger name, start and end times, with fields already named to match timevis() argument requirements

3: Timeline from a dataframe containing only start (field visit) times and different column (field) names than timevis arguments.  Have to determine end time.

4: Same data as in # 3, but displaying the opposite, sites associated with each logger.

For more information on timevis(), see:
https://github.com/daattali/timevis#slightly-more-advanced-examples

Timevis() is also desgined for use in shiny apps and can be interactive.

A red line automatically appears that marks the current time.  I don't know how to delete it yet.

Leah Ettema
ettema.leah@epa.gov
