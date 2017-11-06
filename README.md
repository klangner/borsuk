# Borsuk

Server for Machine Learning models.
This server mangaes datasets and build prediction models which ca be accessed via REST HTTP API.

 
## Running the server
 
 ```bash
sbt assembly
java -jar target/scala-2.12/borsuk.jar 
 ```
 
```bash
curl --data 'dataset=console&data={"ts":"2017"}' http://localhost:7074/api/data

``` 

# Redistributing

Dataset Server source code is distributed under the Apache-2.0 license.

**Contributions**

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
licensed as above, without any additional terms or conditions.

This project is named after great Polish mathematician [Karol Borsuk](https://en.wikipedia.org/wiki/Karol_Borsuk)
