# Borsuk

[![Build status](https://travis-ci.org/carldata/borsuk.svg?branch=master)](https://travis-ci.org/carldata/borsuk)

Machine Learning server for Time Series data.


## API

#### Create model

URI:
```
POST /api/model/
```

Payload:
```json
{ "type": "prediction"}
```

Supported model types:

  * prediction - Predicts next values
  * anomalies - Find anomalies and approximate correct values

Return:
```json
{ "id": "id"}
```

 
#### Fit model

URI:
```
POST /api/model/<id>/fit
```

Payload:
```json
{
  "features": [[1,2,3], [3,4,5]],
  "labels": [1,2,1]
}
```

Where:
  * features - matrix with each feature as a column and each row is single data point
  * labels - List of target values

Return:
HTTP OK 
 
Pleas keep in mind that the fitting process can take lots of time. This function will start learning process and return
immediately.
 

#### Check model status

Check model status. 
Since model is training in asynchronous mode, this function can be used to check which model is currently served  

URI:
```
GET /api/model/<id>/status
```

Return:
```json
{
  "build": "1"
}
```

Model build number which is currently served
 
 
## Running the server
 
 ```bash
sbt assembly
java -jar target/scala-2.12/borsuk.jar 
 ```

 
#### Predict

Use model to predict labels based on the features

URI:
```
POST /api/model/<id>/predict
```

Payload:
```json
{
  "features": [[1,2,3], [3,4,5]]
}
```

Return:
```json
{
  "labels": [1,2,3]
}
```
 

 
# Redistributing

Dataset Server source code is distributed under the Apache-2.0 license.

#### Contributions

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
licensed as above, without any additional terms or conditions.

This project is named after great Polish mathematician [Karol Borsuk](https://en.wikipedia.org/wiki/Karol_Borsuk)
