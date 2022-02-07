package com.example.kotlinlogin

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import android.view.View
import android.widget.EditText
import android.widget.TextView
import com.android.volley.Request
import com.android.volley.Response
import com.android.volley.toolbox.JsonObjectRequest
import org.json.JSONArray
import org.json.JSONObject
import android.os.Handler
import android.os.Looper

class MainActivity3 : AppCompatActivity() {
    var qResult:JSONArray? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main3)

        val query = "SELECT users.id, users.username, roles.role FROM `users` INNER JOIN roles ON users.role_id = roles.id WHERE users.username='${MainActivity.username}'"
        qResult = null
        queryDB(MainActivity.username,MainActivity.password,query)
        val user_result = qResult

        Handler(Looper.getMainLooper()).postDelayed({
            //Do something after 1000ms
            val tv = findViewById<TextView>(R.id.textView)
            if (qResult==null) {
                tv.text = "Cos poszlo nie tak!"
            }
            else {
                tv.text = "Jestes zalogowany do sklepu jako \n"+qResult.toString()
            }
        }, 1000)
    }


    fun queryResult(result : JSONObject) {
        val jsonArray = JSONArray(result["message"].toString())
        qResult = jsonArray
    }

    fun queryDB(user:String,password:String,query:String) {
        // Post parameters
        val jsonObject = JSONObject()
        jsonObject.put("username",user)
        jsonObject.put("password",password)
        jsonObject.put("email","")
        jsonObject.put("query",query)

        // Volley post request with parameters
        val url = "http://10.0.2.2/androiddb/"
        val requestPOST = JsonObjectRequest(Request.Method.POST,url,jsonObject,
            Response.Listener { response ->
                // Process the json
                try {

                    Log.d("fun queryDB:","Response: $response")
                    queryResult(response)
                }catch (e:Exception){
                    Log.d("fun queryDB:","Exception: $e")
                }

            }, Response.ErrorListener{
                // Error in request
                Log.d("fun queryDB:","Volley error: $it")
            })

        VolleySingleton.getInstance(this).addToRequestQueue(requestPOST)
    }



    fun onClickQuery_all(v: View) {

        val  user = MainActivity.username
        val  password = MainActivity.password
        val query = "SELECT * FROM item"
        val url = "http://10.0.2.2/androiddb/"


        // Post parameters
        val jsonObject = JSONObject()
        jsonObject.put("username",user)
        jsonObject.put("password",password)
        jsonObject.put("email","")
        jsonObject.put("query",query)

        Log.d("fun onClickQuery:","jsonObject: $jsonObject")

        // Volley post request with parameters
        val requestPOST = JsonObjectRequest(Request.Method.POST,url,jsonObject,
            Response.Listener { response ->
                // Process the json
                try {

                    Log.d("fun onClickQuery:","Response: $response")
                    printResult(response)
                }catch (e:Exception){
                    Log.d("fun onClickQuery:","Exception: $e")
                }

            }, Response.ErrorListener{
                // Error in request
                Log.d("fun onClickQuery:","Volley error: $it")
            })

        VolleySingleton.getInstance(this).addToRequestQueue(requestPOST)

    }
    fun onClickQuery_buy(v: View) {

        val  user = MainActivity.username
        val  password = MainActivity.password
        val  item_to_buy_id = findViewById<EditText>(R.id.editTextQuery).text.toString()
        val  user_to_buy_id = findViewById<EditText>(R.id.editTextQuery2).text.toString()
        val  query = "INSERT INTO `basket` (`id`, `user_id`, `item_id`) VALUES (NULL, '${user_to_buy_id}', '${item_to_buy_id}')"
        val url = "http://10.0.2.2/androiddb/"


        // Post parameters
        val jsonObject = JSONObject()
        jsonObject.put("username",user)
        jsonObject.put("password",password)
        jsonObject.put("email","")
        jsonObject.put("query",query)

        Log.d("fun onClickQuery:","jsonObject: $jsonObject")

        // Volley post request with parameters
        val requestPOST = JsonObjectRequest(Request.Method.POST,url,jsonObject,
            Response.Listener { response ->
                // Process the json
                try {

                    Log.d("fun onClickQuery:","Response: $response")
                    printResult(response)
                }catch (e:Exception){
                    Log.d("fun onClickQuery:","Exception: $e")
                }

            }, Response.ErrorListener{
                // Error in request
                Log.d("fun onClickQuery:","Volley error: $it")
            })

        VolleySingleton.getInstance(this).addToRequestQueue(requestPOST)

    }
    fun onClickQuery_rm(v: View) {

        val  user = MainActivity.username
        val  password = MainActivity.password
        val  item_to_remove = findViewById<EditText>(R.id.editTextQuery).text.toString()
        val  query = "DELETE FROM `basket` WHERE `basket`.`id` = '${item_to_remove}'"
        val url = "http://10.0.2.2/androiddb/"


        // Post parameters
        val jsonObject = JSONObject()
        jsonObject.put("username",user)
        jsonObject.put("password",password)
        jsonObject.put("email","")
        jsonObject.put("query",query)

        Log.d("fun onClickQuery:","jsonObject: $jsonObject")

        // Volley post request with parameters
        val requestPOST = JsonObjectRequest(Request.Method.POST,url,jsonObject,
            Response.Listener { response ->
                // Process the json
                try {

                    Log.d("fun onClickQuery:","Response: $response")
                    printResult(response)
                }catch (e:Exception){
                    Log.d("fun onClickQuery:","Exception: $e")
                }

            }, Response.ErrorListener{
                // Error in request
                Log.d("fun onClickQuery:","Volley error: $it")
            })

        VolleySingleton.getInstance(this).addToRequestQueue(requestPOST)


    }
    fun onClickQuery_basket(v: View) {

        val  user = MainActivity.username
        val  password = MainActivity.password
        val query = "SELECT basket.id, name, size, gender FROM basket LEFT JOIN item ON basket.item_id = item.id LEFT JOIN users ON basket.user_id = users.id WHERE users.username='${MainActivity.username}'"
        val url = "http://10.0.2.2/androiddb/"


        // Post parameters
        val jsonObject = JSONObject()
        jsonObject.put("username",user)
        jsonObject.put("password",password)
        jsonObject.put("email","")
        jsonObject.put("query",query)

        Log.d("fun onClickQuery:","jsonObject: $jsonObject")

        // Volley post request with parameters
        val requestPOST = JsonObjectRequest(Request.Method.POST,url,jsonObject,
            Response.Listener { response ->
                // Process the json
                try {

                    Log.d("fun onClickQuery:","Response: $response")
                    printResult(response)
                }catch (e:Exception){
                    Log.d("fun onClickQuery:","Exception: $e")
                }

            }, Response.ErrorListener{
                // Error in request
                Log.d("fun onClickQuery:","Volley error: $it")
            })

        VolleySingleton.getInstance(this).addToRequestQueue(requestPOST)

    }

    fun printResult(result : JSONObject) {
        val jsonArray = JSONArray(result["message"].toString())
        var output = "Ilosc wierszy:"+jsonArray.length()+"\n"
        for (i in 0 until jsonArray.length())
            output+="$i-->"+jsonArray[i].toString()+"\n"

        findViewById<TextView>(R.id.resultTextView).text = output
    }

}