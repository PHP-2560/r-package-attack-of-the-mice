#Capitalizes the first letter in a word
capitalize_first_letter = function(x)
{
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}