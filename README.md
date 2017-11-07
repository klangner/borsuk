# Borsuk

Server for Machine Learning models.
This server manages datasets and build prediction models which can be accessed via REST HTTP API.

 
## Running the server
 
 ```bash
sbt assembly
java -jar target/scala-2.12/borsuk.jar 
 ```
 
Add sample to the "test" dataset (which is of type TimeSeriesModel) 
```bash
curl -H "Content-Type: application/json" -X POST -d '{"index":"2017", "value": 12.3}' http://localhost:7074/model/test
``` 

Get next value prediction
```bash
curl http://localhost:7074/model/test/prediction
``` 

 
## Datasets

Datasets are stored on the file system using scheme:
<model_name>/<model_name>-<date>.csv
 

# Redistributing

Dataset Server source code is distributed under the Apache-2.0 license.

**Contributions**

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
licensed as above, without any additional terms or conditions.

This project is named after great Polish mathematician [Karol Borsuk](https://en.wikipedia.org/wiki/Karol_Borsuk)
