#Time Interval Data Analyzer
[![Build Status](https://travis-ci.org/pmeisen/dis-timeintervaldataanalyzer.svg?branch=master)](https://travis-ci.org/pmeisen/dis-timeintervaldataanalyzer)

This project creates the back-end of the Time Interval Data Server. It is mentioned in the book [Time Interval Data Analysis](https://www.amazon.com/Analyzing-Time-Interval-Data-Introducing/dp/3658157275).
For further insights regarding the provided analytical capacities of the TIDAInformationSystem, the used TIDAModel and TIDAQueryLanguage have a look at the book or at the following research papers:

- [TIDAModel](https://www.researchgate.net/publication/266733554_Modeling_and_Processing_of_Time_Interval_Data_for_Data-Driven_Decision_Support)
- [TIDAQueryLanguage](https://www.researchgate.net/publication/275828232_TIDAQL_A_Query_Language_enabling_On-line_Analytical_Processing_of_Time_Interval_Data)
- [Indexing](https://www.researchgate.net/publication/274897254_Bitmap-Based_On-Line_Analytical_Processing_of_Time_Interval_Data)
- [Similarity](https://www.researchgate.net/publication/283712168_Similarity_Search_of_Bounded_TIDASETs_within_Large_Time_Interval_Databases)

If you'd like to test, play with, or develop the TIDAPlatform, please have a look at the [umbrella/wrapper project](https://github.com/pmeisen/dis-timeintervaldataanalyzer-assembly).
 
## Back-End Development

When developing the back-end it is recommend to use `junit` tests to ensure the quality of your implementations. There are several important things to know
when developing the back-end (some of it is explained in the book or the publications), nevertheless most of these things are only documented via `JavaDoc` and 
overall concepts are mostly not documented at all (yet, everybody is more than welcome to contribute documentations).

Further down the road, it may be helpful to have the UI available to, e.g., fire some queries against the back-end. To do so, it is possible to simple start the
UI project via `99-run-dist-server` (see the project's `gruntfile.js`). The UI is served via an grunt-internal `HTTP`-server implementation and can be accessed using
`http://localhost:20000/login.html`. To change the back-end the front-end is communicating with, simply select the `Change server` and configure the back-end's url
accordingly to the back-ends HTTP configuration.

<p align="center">
  <img src="https://raw.githubusercontent.com/pmeisen/dis-timeintervaldataanalyzer/master/docs/tida-change-server.png" alt="Change Server" width="230">
</p>

<p align="center">
  <img src="https://raw.githubusercontent.com/pmeisen/dis-timeintervaldataanalyzer/master/docs/tida-server-settings.png" alt="Server Settings" width="460">
</p>

Further documentation regarding the usage of the available `ant-targets`, `troubleshooting`, and many more may be added in the future.