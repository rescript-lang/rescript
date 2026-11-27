export default function(){
    var sum =   0 ; 
    var i = 0;
    for(; i < arguments.length ; ++ i){
        sum += arguments[i]
    }
    return sum
}