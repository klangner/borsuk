package borsuk

import org.scalatest.{WordSpec, Matchers}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

import carldata.borsuk.Routing

class EnvelopeApiTest extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {

  private def mainRoute = {
    val routing = new Routing()
    routing.route()
  }


  "The Envelope" should {

    "create new model" in {

    }

    "return conflict for model already created" in {


    }

    "not fit if model does not exist" in {

    }

    "not list Envelope if model does not exist" in {

    }

    "not give status if model not found" in {

    }

    "fit the model" in {

    }

    "list the existing model" in {


    }

    "get the model" in {

    }

    "not get the model when it does not exits" in {


    }

  }

}
