package com.example.calculator

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.text.Selection
import android.text.SpannableStringBuilder
import android.view.View
import android.widget.Button
import android.widget.TextView
import java.math.RoundingMode

class MainActivity : AppCompatActivity() {

    var liczba1: String = ""
    var liczba2: String = ""
    var wynik: String = ""
    var operator: String = ""
    var isOJustPressed: Boolean = false

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

    }


    fun onClickDigit(view: View) {
        val ow = findViewById<TextView>(R.id.output)
        if (isOJustPressed) {
            ow.text = ""
            isOJustPressed = false
        }
        var tekst: String = "" + ow.text
        if (tekst == "0") tekst = "";
        ow.text = tekst + (view as Button).text
    }

    fun onClickOperator(view: View) {
        val ow = findViewById<TextView>(R.id.output)
        liczba1 = ow.text.toString()
        when (view.id) {
            R.id.addBTN -> operator = "+"
            R.id.minusBTN -> operator = "-"
            R.id.timesBTN -> operator = "*"
            R.id.divideBTN -> operator = "/"
            R.id.pointBTN -> operator = "."
            R.id.plusMinusBTN -> operator = "+/-"
        }
        isOJustPressed = true
    }


    fun onClickEqual(view: View) {
        val ow = findViewById<TextView>(R.id.output)
        liczba2 = ow.text.toString()
        when (operator) {
            "+" -> wynik = add(liczba1, liczba2)
            "-" -> wynik = sub(liczba1, liczba2)
            "*" -> wynik = mul(liczba1, liczba2)
            "/" -> wynik = div(liczba1, liczba2)
            "." -> wynik = dot(liczba1, liczba2)
        }
        ow.text = wynik
        liczba1 = ""
        liczba2 = ""
        operator = ""
        isOJustPressed = true
    }

    fun onClickC(view: View) {
        val ow = findViewById<TextView>(R.id.output)
        ow.text = ""
    }

    fun onClickPlusMinus(view: View) {
        val ow = findViewById<TextView>(R.id.output)
        liczba1 = ow.text.toString()
        wynik = reverse(liczba1)
        ow.text = wynik

    }

    fun onClickBackspace(view: View) {
        val ow = findViewById<TextView>(R.id.output)
        val cursorPosition = ow.selectionStart
        val textLen = ow.text.length
        if (cursorPosition != 0 && textLen != 0) {
            val selection = SpannableStringBuilder(ow.text)
            selection.replace(cursorPosition - 1, cursorPosition, "")
            ow.text = selection
        }

    }


    private fun add(l1: String, l2: String): String {
        return (l1.toBigDecimal() + l2.toBigDecimal()).toPlainString()
    }

    private fun sub(l1: String, l2: String): String {
        return (l1.toBigDecimal() - l2.toBigDecimal()).toPlainString()
    }

    private fun mul(l1: String, l2: String): String {
        return (l1.toBigDecimal() * l2.toBigDecimal()).toPlainString()
    }

    private fun div(l1: String, l2: String): String {
        return l1.toBigDecimal().divide(l2.toBigDecimal(), 10, RoundingMode.HALF_UP).toPlainString()
    }

    private fun dot(l1: String, l2: String): String {
        var divisor = 10.toBigDecimal()
        var doted = l2.toBigDecimal().divide(divisor, 10, RoundingMode.HALF_UP)
        return (l1.toBigDecimal() + doted).toPlainString()
    }

    private fun reverse(l1: String): String {
        return "-" + l1.toBigDecimal()
    }

}