#
#' Envoie une requête dans le datalab STM
#'
#' @param requete chaine de caractères, la requete 
#' @param type character, type de requete (connect, query, save)
#' @param odbc booleen qui indique si on se connecte en odbc ou en jdbc
#' @param data.frame nom de la data frame a monter dans la base (save)
#' @param table_name nom de la table dans stm qui correspond au dataframe (save)
#' @param uid character, uid
#' @param pwd , character, mot de passe 
#' @param f nom du fichier dans lequel est inscrit le mot de passe: 
#' null par défaut : recherche dans mdp.txt a la racine, et s'il n existe pas, 
#' @param append est ce qu'on doit mettre a la suite des autres les resultats
#' @param rownames 
#' @param datasource nom de la base de donnees en odbc, et STM_PROD (defaut) ou STM_PREPROD sur le serveur 
#' @param lab character, nom du lab auquel se connecter en jdbc 
#' @import RODBC
#' @import RJDBC
#' @import tcltk
#' 
#' @details connection odbc si local, jdbc si serveur par defaut  
#' Pour la recherche du mot de passe si pas fourni, cherche dans le fichier f si 
#' fourni, sinon dans 
#' @seealso get_pwd
#' @return le resultat de la requete 
#' @export
#'
#' @examples 
#' ##exemple en local : 
#' \dontrun{
#'  goSTM("select top 5 * from LABS_NEDI_072_DISC_COP.uu_2010", datasource = "STM_PPRD")
#' }
#' 
#'##exemples sur le serveur : 
#'\dontrun{
#' #par defaut : STM_PROD, lab labs_STM_DLAB_DISC_NEDI_prd
#'goSTM("select top 5 * from uu_2010") 
#'goSTM("select top 5 * from V_A_MSR_HIST_CONS", 
#' lab = "STM_V_DETL_TOTL_HIST_PRD", datasource = "STM_PROD")
#' goSTM("select top 5 * from V_A_MSR_HIST_CONS", 
#' lab = "STM_V_DETL_TOTL_HIST_cop", datasource = "STM_PREPROD")
#'}

goSTM <- function(requete="",
                  type=c("Connect","Query","Save")[2],
                  odbc = is_local(),
                  data.frame="",
                  table_name="",
                  uid=get_uid(),
                  pwd = NULL,
                  f = NULL,
                  lab=NULL,
                  append=FALSE,
                  rownames=FALSE,
                  dec = getOption("dec"),
                  datasource="STM_PROD")
{

  if(is.null(pwd)){pwd<-get_pwd(f)}
  
	if(odbc)
	{
	  library(RODBC)
		db <- odbcConnect(datasource, uid = uid, pwd = pwd)
		
		if(type == "Connect")
		{
			return(db)
		}
		if(type == "Query")
		{
			if(length(requete)==1)
			{
			 
				Sortie <- sqlQuery(db,requete, dec = dec)
			}else{
				Sortie <-list()
				for(i in 1:length(requete))
				{
					Sortie[[i]]<-sqlQuery(db,requete[i], dec = dec)
				}
			}
			odbcClose(db)
			return(Sortie)
		}
		if(type == "Save")
		{
		  if(!append){
# 		    e<-sqlQuery(db,paste("select tablename from dbc.tables where tablename='",table_name,"'",sep=""))
# 		    if(e==0)append=F
# 		  }
  		  for(j in 1:ncol(data.frame)){
  		    varname<-names(data.frame)[j]
  		    if(inherits(data.frame[,j], "Date"))varname<-paste(varname,"date")
  		    else if(inherits(data.frame[,j], "POSIXct"))varname<-paste(varname,"timestamp")
  		    else if(typeof(data.frame[,j]) %in% c("integer","double"))varname<-paste(varname,"float")else varname<-paste(varname,"character(",max(c(14,nchar(data.frame[,j]))),")")
  		    if(j==1)varnames<-varname else varnames<-paste(varnames,",",varname)
  		  }
  		  req<-paste("create table ",table_name," ( ",varnames,")",sep="")
  		  #print(req)
  		  sqlQuery(db,req, dec = dec)
		  }
		  sqlSave(db,data.frame,table_name,append=T,rownames=rownames)
			odbcClose(db)			
		}
	}else{

    require(RJDBC)
    options(java.parameters='-Xmx10g')
    
		server = switch(datasource
		                , STM_PREPROD = 'bdd_stm_pp'
		                , STM_PROD = 'bdd_stm_prd')
		
		if(is.null(lab))
		{
		  
		  lab = switch(datasource
		               , STM_PREPROD = 'STM_V_DETL_TOTL_HIST_PRD'
		               , STM_PROD = 'labs_STM_DLAB_DISC_NEDI_prd')
		}
    
		drv <- JDBC(driverClass = "com.teradata.jdbc.TeraDriver", 
                classPath = "/usr/bin/terajdbc4.jar:/usr/bin/tdgssconfig.jar")
		db <- dbConnect(drv,paste0("jdbc:teradata://", 
                               server, ".distribution.edf.fr/DATABASE=",lab,", LOGMECH=LDAP, TYPE=FASTEXPORT"), uid, pwd)
		
		if(type == "Connect")
		{
			return(db)
		}
		#####MODIF ANNE: ON REGARDE SI C EST UNE LISTE OU NON 
		##SI C EST UNE LISTE
		
		if(type == "Send"){
		  dbSendQuery(db,requete)
		}
		if(type == "Query")
		  {
			if(length(requete)==1)
			{
				Sortie<-dbGetQuery(db,requete)
			}else{
				Sortie<-list()
				for(i in 1:length(requete)){
				 # for(i in 19:length(requete)){
				  print(i)
				  statem <- requete[[i]]
				  ##modif ici : si jamais c est une creation de table, on doit faire un dbSend et pas 
				  ##un dbGet
				  if (is_send_statement(statem )){
				    dbSendQuery(db, statem )
				  }else{
				    Sortie[[i]]<-dbGetQuery(db,statem )
				  }

				}
			}
			dbDisconnect(db)
			#odbcClose(db)
			return(Sortie)
		}
		if(type == "Save")
		{
			table_name2<-substr(table_name,regexpr(".",table_name,fixed=T)+1,nchar(table_name))
			if(dbExistsTable(db,table_name2)){

			  if(!append){

				dbSendQuery(db,paste("drop table ",table_name2,sep=""))

			  }else{
				fields<-dbGetFields(db,table_name2)
				fm<-fields$COLUMN_NAME[!fields$COLUMN_NAME %in% names(data.frame)]
				for(fm1 in fm)data.frame[,fm1]<-NA
				data.frame<-data.frame[,fields$COLUMN_NAME]
			  }
			}else{
			  print("la table n existe pas ")
			  if(typeof(data.frame[,1]) %in% c("integer","double"))varnames<-paste(names(data.frame)[1]," numeric",sep="")else varnames<-paste(names(data.frame)[1]," character(14)",sep="")
			  for(j in 2:ncol(data.frame)){
			    if(typeof(data.frame[,j]) %in% c("integer","double"))varnames<-paste(varnames,",",names(data.frame)[j]," numeric",sep="")else varnames<-paste(varnames,",",names(data.frame)[j]," character(14)",sep="")
			  }
			  req<-paste("create table ",table_name," ( ",varnames,")",sep="")
			  paste(req)
			  dbSendQuery(db,req)
			}
			dbWriteTable(db,table_name2,data.frame,append=T,overwrite=F)
			#odbcClose(db)
			dbDisconnect(db)
		}
	}
	rm(pwd)
}




#' Recupere le user id a partir des variables d environnement 
#' @return user id 
get_uid <- function(){
  
  uid <- Sys.getenv("USERNAME")
  if (uid == ""){
    uid <- Sys.getenv("USER")
    uid <-  substr(uid, 3, nchar(uid)) 
  }
  toupper(uid)
}


#' Recupere le mot de passe situe dans un fichier 
#' @param f chemin du fichier (string). Si NULL, (par defaut), il tente de deviner
#' en cherchant le fichier "mdp.txt"
#' @import tcltk
#' @return le mot de passe (string)
get_pwd <- function(f = NULL){
  ##localisation par defaut du mot de passe: mdp.txt dans le home ou mes documents
  if (is.null(f)){
    f <- if (is_local()){
      paste0("C:/Users/"  , get_uid(), "/Documents/R/mdp.txt")
  }else{"~/mdp.txt"}
      
  }
  
  if ( file.exists(f)){
    pwd   <- suppressWarnings(readLines(f))#as.character
    pwd <- gsub(pwd, pattern = "DataLab:",replacement = "")
  }else{
    tt<-tktoplevel()
    tktitle(tt)<-"Bienvenue"
    titre1<-tklabel(tt, text="Veuiller saisir votre mot de passe :")
    tkpack(titre1)
    temp<-tkentry(tt, width=1, textvariable=tclVar(""))
    tkpack(temp)
    ok<-function()
    {
      pwd<<-tclvalue(tkget(temp))
      .Tcl("set exit_to_quit 1")
      tkdestroy(tt)
    }
    bouton<-tkbutton(tt, text="Se connecter au Datalab STM", command=ok)
    tkpack(bouton)
    .Tcl("set exit_to_quit 0")
    .Tcl("wait exit_to_quit")  		
    rm("bouton","ok","temp" ,"titre1","tt")
  }
  
  pwd
}


#' fonction qui detecte si on est en local ou sur le serveur 
is_local <- function(){
  nni_local <- (Sys.getenv("USERNAME") != "")
  nni_server <-( Sys.getenv("USER") != "")
  if (nni_local  == nni_server){
    NA
  }else{
   nni_local
  }
}
