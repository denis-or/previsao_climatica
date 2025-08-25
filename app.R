source("utils.R")

cli::cli_alert_info(format(Sys.time(), "%d/%m/%Y %H:%M:%S"))

ui <- bslib::page_fluid(
	theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
	autoWaiter(
		color = "transparent",
		html = waiter::spin_loaders(
			id = 6,
			color = "#243163",
			style = "height: 80px; width: 80px; border-radius: 80%;"
		)
	),

	tags$script(
		HTML(
			"
    $(document).ready(function() {
      $('.card.bslib-card bslib-tooltip > div').each(function() {
        if ($(this).text().includes('Expand')) {
          $(this).text('Ampliar');
        }
      });

      var observer = new MutationObserver(function(mutations) {
        $('.bslib-full-screen-exit').each(function() {
          if ($(this).html().includes('Close')) {
            $(this).html($(this).html().replace('Close', 'Fechar'));
          }
        });
      });

      $('.card.bslib-card').each(function() {
        observer.observe(this, {
          attributes: true,
          attributeFilter: ['data-full-screen']
        });
      });
    });
  "
		)
	),
	bslib::page_navbar(
		window_title = "Clima",
		id = "tabs",
		selected = "Previsão por modelo e indicador climático",
		navbar_options = bslib::navbar_options(
			bg = "#243163",
			collapsible = FALSE,
			theme = "dark"
		),

		bslib::nav_panel(
			title = "Previsão por modelo e indicador climático",
			accordion(
				accordion_panel(
					icon = fa("filter", fill = "#243163"),
					title = " Filtros",
					value = "tets",
					bslib::layout_columns(
						selectInput(
							inputId = "modelo_selecionado",
							label = "Selecione o modelo",
							# choices = "Desabilitado",
							choices = c(
								"ETA Model" = "eta",
								"Brazilian Global Atmospheric Model (BAM)" = "bam",
								"Brazilian developments on the Regional Atmospheric Modeling System 8km (BRAMS)" = "brams8",
								"Brazilian developments on the Regional Atmospheric Modeling System 15km(BRAMS)" = "brams15",
								"Weather Research and Forecasting (WRF)" = "wrf",
								"Multimodel" = "multimodelo"
							),
							selected = "ETA Model"
						),
						selectInput(
							"indicador_selecionado",
							label = "Selecione o indicador",
							choices = c(
								"Temperatura" = "temp",
								"Precipitação" = "prec"
							),
							selected = "Temperatura"
						),
						selectInput(
							"municipio_selecionado",
							label = "Selecione o município",
							choices = rj_cptec$nome_mun,
							selected = "Rio de Janeiro"
						)
					)
				)
			),
			card(
				card_header("Previsão para os próximos dias"),
				# textOutput("modelo_"),
				# textOutput("municipio_"),
				# textOutput("indicador_"),
				card_body(echarts4r::echarts4rOutput(outputId = "grafico_previsao"))
			)
		),
		bslib::nav_panel(
			title = "Nota metodológica"
		)
	)
)

server <- function(input, output, session) {
	base_modelo <- reactive({
		req(input$modelo_selecionado)
		req(input$municipio_selecionado)

		pega_dados_modelo(
			modelo = input$modelo_selecionado,
			municipio = input$municipio_selecionado
		)
	})

	# output$modelo_ <- renderText({ input$modelo_selecionado })
	# output$municipio_ <- renderText({ input$municipio_selecionado })
	# output$indicador_ <- renderText({ input$indicador_selecionado |> class() })

	output$grafico_previsao <- echarts4r::renderEcharts4r({
		req(base_modelo())

		if (length(base_modelo()) < 5) {
			data.frame(x = 1, y = 1) |>
				echarts4r::e_charts(x) |>
				echarts4r::e_title(
					text = "Não há dados estimados para esse modelo no momento atual.",
					left = "center",
					top = "middle",
					textStyle = list(fontSize = 16, color = "#747474")
				) |>
				echarts4r::e_x_axis(show = FALSE) |>
				echarts4r::e_y_axis(show = FALSE) |>
				echarts4r::e_grid(
					left = 0,
					right = 0,
					top = 0,
					bottom = 0,
					show = FALSE
				) |>
				echarts4r::e_tooltip(FALSE) |>
				echarts4r::e_legend(FALSE)
		} else {
			echarts4r::e_charts(data = base_modelo(), x = date) |>
				echarts4r::e_line_(
					serie = input$indicador_selecionado,
					name = names(input$indicador_selecionado),
					legend = FALSE,
					lineStyle = list(
						width = 3,
						color = "#4472C4"
					),
					symbol = "none",
					showSymbol = FALSE
				) |>
				echarts4r::e_tooltip(
					trigger = "axis",
					formatter = e_tooltip_pointer_formatter(
						'decimal',
						digits = 2,
						locale = "pt-BR"
					),
					textStyle = list(
						fontSize = 20
					),
					axisPointer = list(
						type = "shadow"
					)
				) |>
				# Toolbox
				echarts4r::e_datazoom(type = "slider", start = 0, end = 100) |>
				echarts4r::e_grid(height = "80%", top = "5%", width = "85%") |>
				echarts4r::e_text_style(fontSize = 20) |>
				echarts4r::e_toolbox(
					itemSize = 30,
					orient = "vertical",
					fontSize = 20
				) |>
				echarts4r::e_toolbox_feature(
					feature = "dataView",
					title = "Copiar dados desse gráfico",
					lang = list(
						'Selecione os dados abaixo e use seu CTRL+C',
						'Fechar',
						'Atualizar'
					)
				) |>
				echarts4r::e_toolbox_feature(
					feature = "saveAsImage",
					title = "Salvar como imagem (PNG)"
				) |>
				echarts4r::e_show_loading()
		}
	})
}

shinyApp(ui, server)
