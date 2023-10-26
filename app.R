library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(stringr)
library(gtools)
library(Rsolnp)
library(geoloc)
library(countrycode)
library(dplyr)
library(rdrop2)

sources = read.table("Sources.txt", sep=",", head=FALSE)
colnames(sources) = c("Fert", "N", "P", "K", "S")
drop_auth(rdstoken = "token.rds")
outputDir = "Fertilizer_blends"

saveLog <- function(Log, fileName="logs.csv"){
	logs = drop_read_csv(paste0(outputDir,"/", fileName), stringsAsFactors=FALSE)
	logs %>% rbind(Log) %>% write.csv(fileName, row.names=FALSE)
	
	drop_upload(fileName, path = outputDir)
	on.exit(file.remove(fileName))
	on.exit(remove(Log))
	on.exit(remove(logs))
}

ui <-
tagList(
	fluidPage(
		useShinyjs(),
		tags$head(includeHTML(("google-analytics.html"))),
		tags$div(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
		theme=shinytheme("spacelab"),
			titlePanel("NPK blends calculator by UFCL"),
			sidebarLayout(sidebarPanel(
				h3("Blend composition"),
				h5("Enter desired values for N, P, K and S"),
				splitLayout(align="center",
					numericInput("N", "N", value=25),
					numericInput("P", "P", value=5),
					numericInput("K", "K", value=10),
					numericInput("S", "S", value=3)
				),
				h3("Blend components"),
				splitLayout(align="left", cellWidths = c("75%", "25%"),
					verticalLayout(
						h5("Select number of components"),
						sliderInput("comp", NULL,
							min=2, max=4, value=3, step=1, width = "75%", ticks=FALSE
						)
					),
					verticalLayout(
						h5("With filler?"),
						prettySwitch("filler", textOutput("yn_1"), fill = TRUE, status = "primary")
						)
				),
				h5("Select components from drop down lists or customize"),
				splitLayout(align="center", 
					verticalLayout(
						selectInput("fert_1", NULL, choices=paste(sources$Fert[1:17]), selected="AmSul"),
						conditionalPanel(condition="input.fert_1.indexOf('Custom')>-1 ",
							textInput("cust_1", "N:P:K:S", "0:0:0:0")
						)
					),
					verticalLayout(
						selectInput("fert_2", NULL, choices=paste(sources$Fert[1:17]), selected="DAP"),
						conditionalPanel(condition="input.fert_2.indexOf('Custom')>-1 ",
							textInput("cust_2", "N:P:K:S", "0:0:0:0")
						)
					),
					conditionalPanel(condition="input.comp>2",
						selectInput("fert_3", NULL, choices=paste(sources$Fert[1:15]), selected="MOP")
					),
					conditionalPanel(condition="input.comp>3",
						selectInput("fert_4", NULL, choices=paste(sources$Fert[1:15]), selected="Urea")
					)
				),
				h3("Enter price of raw materials"),
				splitLayout(align="center", 
					numericInput("price_1", textOutput("txt_1"), value=0),
					numericInput("price_2", textOutput("txt_2"), value=0),
					conditionalPanel(condition="input.comp>2",
						numericInput("price_3", textOutput("txt_3"), value=0)
					),
					conditionalPanel(condition="input.comp>3",
						numericInput("price_4", textOutput("txt_4"), value=0)
					),
					conditionalPanel(condition="input.filler==1",
						numericInput("price_5", "Filler", value=0)
					)
				),
				h3("Enter blend volume"),
				numericInput("vol", label=NULL, value=1000),
				actionButton("generate", "Click to calculate blend", class = "btn-primary")
			),
				mainPanel(
				fluidRow(column(10,
					fluidRow(style='border: 1px solid grey; border-radius: 5px; background-color: #E2EFDA; margin-bottom: 1%',
						column(5, align="center", style='width: 50%', h4(strong("Your blend recipe")),
						tableOutput("table_1.1")),
						column(5, align="center", style='width: 50%', h4(strong("Chemical composition of the blend")),
						tableOutput("table_1.2"))
						)
				)),
				fluidRow(
					htmlOutput("blend_price_per_unit"),
					htmlOutput("blend_price_total")
				)
				)
			),
	uiOutput("Border")
	)
)

server <-
function(input, output, session){
	
	output$yn_1 <- renderText({ifelse(input$filler, "yes", "no")})
	output$txt_1 <- renderText({ ifelse(str_detect(input$fert_1, "Custom"), input$cust_1, input$fert_1) })
	output$txt_2 <- renderText({ ifelse(str_detect(input$fert_2, "Custom"), input$cust_2, input$fert_2) })
	output$txt_3 <- renderText({ input$fert_3 })
	output$txt_4 <- renderText({ input$fert_4 })
	
	observe <- observeEvent(input$generate,{
	
		MSE <- function(x){
			if(input$comp<3){x[3:4]=0}
			if(input$comp!=4){x[4]=0}
			((input$N-(sources[index1,"N"]*x[1]+sources[index2,"N"]*x[2]+sources[index3,"N"]*x[3]+sources[index4,"N"]*x[4]+sources[index5,"N"]*x[5]))^2+
			(input$P-(sources[index1,"P"]*x[1]+sources[index2,"P"]*x[2]+sources[index3,"P"]*x[3]+sources[index4,"P"]*x[4]+sources[index5,"P"]*x[5]))^2+
			(input$K-(sources[index1,"K"]*x[1]+sources[index2,"K"]*x[2]+sources[index3,"K"]*x[3]+sources[index4,"K"]*x[4]+sources[index5,"K"]*x[5]))^2+
			ifelse(is.na(input$S),0,(input$S-(sources[index1,"S"]*x[1]+sources[index2,"S"]*x[2]+sources[index3,"S"]*x[3]+sources[index4,"S"]*x[4]+sources[index5,"S"]*x[5]))^2)
			)/4
		}
		
		constr <- function(x){sum(x[1:input$comp], ifelse(input$filler==1,x[5],0))}
		
		index1 = sources$Fert %in% input$fert_1
		index2 = sources$Fert %in% input$fert_2
		index3 = sources$Fert %in% input$fert_3
		index4 = sources$Fert %in% input$fert_4
		index5 = sources$Fert %in% "Filler"
		
		
		if (str_detect(input$fert_1, "Custom")){
			for (i in seq(2,5)){
				sources[index1,i] = as.numeric(str_split(input$cust_1, ":")[[1]][i-1])
			}
		}
		
		if (str_detect(input$fert_2, "Custom")){
			for (i in seq(2,5)){
				sources[index2,i] = as.numeric(str_split(input$cust_2, ":")[[1]][i-1])
			}
		}
		
		solve = solnp(pars=rep(0.2,5), fun=MSE, eqfun=constr, eqB=1, LB=rep(0,5), UB=rep(1,5))
		comb = data.frame(perc_1 = solve$pars[1], perc_2 = solve$pars[2], perc_3 = ifelse(input$comp<3, 0, solve$pars[3]), perc_4 = ifelse(input$comp!=4, 0, solve$pars[4]), perc_5 = ifelse(input$filler!=1,0,solve$pars[5]))
		blend = data.frame(
			one = input$vol*comb$perc_1,
			two = input$vol*comb$perc_2,
			thr = input$vol*comb$perc_3,
			fou = input$vol*comb$perc_4,
			fiv = input$vol*comb$perc_5,
			N = with(comb, sources[index1,"N"]*perc_1+sources[index2,"N"]*perc_2+sources[index3,"N"]*perc_3+sources[index4,"N"]*perc_4+sources[index5,"N"]*perc_5) %>% round(digits=1) %>% format(nsmall=1),
			P = with(comb, sources[index1,"P"]*perc_1+sources[index2,"P"]*perc_2+sources[index3,"P"]*perc_3+sources[index4,"P"]*perc_4+sources[index5,"P"]*perc_5) %>% round(digits=1) %>% format(nsmall=1),
			K = with(comb, sources[index1,"K"]*perc_1+sources[index2,"K"]*perc_2+sources[index3,"K"]*perc_3+sources[index4,"K"]*perc_4+sources[index5,"K"]*perc_5) %>% round(digits=1) %>% format(nsmall=1),
			S = with(comb, sources[index1,"S"]*perc_1+sources[index2,"S"]*perc_2+sources[index3,"S"]*perc_3+sources[index4,"S"]*perc_4+sources[index5,"S"]*perc_5) %>% round(digits=1) %>% format(nsmall=1)
		)
		names(blend)[1] = as.character(ifelse(str_detect(input$fert_1, "Custom"), input$cust_1, input$fert_1))
		names(blend)[2] = as.character(ifelse(str_detect(input$fert_2, "Custom"), input$cust_2, input$fert_2))
		names(blend)[3] = as.character(sources$Fert[index3])
		names(blend)[4] = as.character(sources$Fert[index4])
		names(blend)[5] = as.character(sources$Fert[index5])
		
		blend_price = comb$perc_1*input$price_1+comb$perc_2*input$price_2+comb$perc_3*input$price_3+comb$perc_4*input$price_4+comb$perc_5*input$price_5 
		blend_price_tot = blend_price*input$vol
		blend_price = blend_price %>% round(digits=2) %>% format(nsmall=1)
		blend_price_tot = blend_price_tot %>% round(digits=2) %>% format(big.mark=" ")
		
		output$table_1.1 <- renderTable({blend[,c(1:input$comp, ifelse(input$filler==1,5,0))]})
		output$table_1.2 <- renderTable({blend[,6:9]})
		output$blend_price_per_unit <- renderText({ paste("<b>Blend price per unit: ", blend_price, "</b>") })
		output$blend_price_total <- renderText({ paste("<b>Total blend price: ", blend_price_tot, "</b>") })
		
		Log = data.frame(
			Date_time = Sys.time() %>% format("%d/%m/%Y %H:%M"),
			Inq_blend = do.call(paste, c(data.frame(input$N, input$P, input$K), sep=":")),
			input_1 = names(blend)[1],
			input_2 = names(blend)[2],
			input_3 = names(blend)[3],
			input_4 = names(blend)[4],
			filler = paste(input$filler),
			Vol = input$vol,
			perc_1 = comb$perc_1,
			perc_2 = comb$perc_2,
			perc_3 = comb$perc_3,
			perc_4 = comb$perc_4,
			perc_5 = comb$perc_5,
			Res_blend = do.call(paste, c(blend[,6:9], sep=":")),
			Inq_country = NA, #geoloc::wtfismyip()$Your_CountryCode %>% countrycode(origin="iso2c", destination="country.name"), 
			IP = NA, #geoloc::wtfismyip()$Your_IPAddress, 
			Dest_country = NA, #input$country, 
			stringsAsFactors = FALSE)
		
		Log %>% saveLog()
		
	})
		
}


shinyApp(ui, server)

