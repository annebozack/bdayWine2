#' Plot function
#'
#' This function generates a plot with linear or exponential growth, with the option of a cat backgroud. The cats package must be installed from https://github.com/hilaryparker/cats (install_github('cats','hilaryparker')) as well as the httr package.
#' @param type Linear or exponential? Default is type = 'linear' (options are type='linear' or type='exponential')
#' @param growth How much does wine effect happiness? Default is growth = 1
#' @param cat Do you want a cat background for your plot? Default is TRUE (options are TRUE or FALSE)
#' @keywords wine
#; @keywords cats
#' @export
#' @examples wine_fun(type='exponential', growth='5')
#' wine_fun_cat()

wine_fun_cat = function(type='linear', growth=1, cat=TRUE){
	funLinear = function(x){
		x*growth
	}
	funExp = function(x){
		x^growth
	}
	p <- ggplot2::ggplot(data = data.frame(x = 0), mapping = aes(x = x))
	if (cat == TRUE){
		if (type == 'linear'){
			p + add_cat(bw = FALSE) + stat_function(fun = funLinear) + xlim(0,100) + labs(x='Wine (glasses)', y='Happiness (N smiles)', title='Relationship between wine consumption and happiness') 
		} else if (type == 'exponential'){
			p + add_cat(bw = FALSE) + stat_function(fun = funExp) + xlim(0,100) + labs(x='Wine (glasses)', y='Happiness (N smiles)', title='Relationship between wine consumption and happiness') 
		} 
	} else if (cat == FALSE){
		if (type == 'linear'){
			p + stat_function(fun = funLinear) + xlim(0,100) + labs(x='Wine (glasses)', y='Happiness (N smiles)', title='Relationship between wine consumption and happiness') 
		} else if (type == 'exponential'){
			p + stat_function(fun = funExp) + xlim(0,100) + labs(x='Wine (glasses)', y='Happiness (N smiles)', title='Relationship between wine consumption and happiness') 
		}
	}
}
