# Borsuk

[![Build status](https://travis-ci.org/carldata/borsuk.svg?branch=master)](https://travis-ci.org/carldata/borsuk)

Server for Time Series analysis

## Features

  * [ ] [Prediction](https://github.com/carldata/borsuk/wiki/Prediction-module)
  * [ ] Anomaly detection
  * [ ] Search
  * Hydrological module
    * [ ] Find storms
    * [ ] RDII analysis


## Running the server
 
 ```bash
sbt assembly
java -jar target/scala-2.12/borsuk.jar 
 ```

 
# Redistributing

Borsuk source code is distributed under the Apache-2.0 license.

#### Contributions

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
licensed as above, without any additional terms or conditions.

This project is named after great Polish mathematician [Karol Borsuk](https://en.wikipedia.org/wiki/Karol_Borsuk)
