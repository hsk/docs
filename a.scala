package a

object main extends App {
var immutableSet = Set(1, 2, 3)   

immutableSet += 4   
//this is the same as:  
immutableSet = immutableSet + 4  

println(immutableSet)  

}
