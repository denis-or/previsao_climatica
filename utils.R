library(httr2)
library(purrr)
library(dplyr)
library(glue)
library(jsonlite)
library(shiny)
library(bslib)
library(waiter)
library(fontawesome)
library(echarts4r)

rj_cptec <- readRDS("rj_cptec.rds")


pega_dados_modelo <- function(
	modelo,
	municipio,
	dia = NULL,
	mes = NULL,
	ano = NULL
) {
	rj_cptec <- readRDS("rj_cptec.rds")

	municipio <- rj_cptec$id_cidade[rj_cptec$nome_mun == municipio]

	dia <- if (is.null(dia)) lubridate::day(Sys.Date())
	mes <- if (is.null(mes)) {
		stringr::str_pad(lubridate::month(Sys.Date()), 2, pad = "0")
	}
	ano <- if (is.null(ano)) lubridate::year(Sys.Date())

	link <- dplyr::case_when(
		modelo == "brams8" ~
			glue::glue(
				"https://s0.cptec.inpe.br/ioper/tempo/BRAMS/ams_08km/recortes/grh/json2/{ano}/{mes}/{dia}/00/{municipio}.json"
			),
		modelo == "bam" ~
			glue::glue(
				"https://s0.cptec.inpe.br/ioper/tempo/BAM/TQ0666L064/recortes/grh/json/{ano}/{mes}/{dia}/00/{municipio}.json"
			),
		modelo == "eta" ~
			glue::glue(
				"https://s0.cptec.inpe.br/ioper/tempo/Eta/ams_08km/recortes/grh/json2/{ano}/{mes}/{dia}/00/{municipio}.json"
			),
		modelo == "brams15" ~
			glue::glue(
				"https://s0.cptec.inpe.br/ioper/produtos/BRAMS/ams_15km/grh/json2/{ano}/{mes}/{dia}/00/{municipio}.json"
			),
		modelo == "multimodelo" ~
			glue::glue(
				"https://s1.cptec.inpe.br/grafico/Modelos/Multimodel/pag_web/horario/{municipio}.json"
			),
		modelo == "wrf" ~
			glue::glue(
				"https://s0.cptec.inpe.br/ioper/tempo/WRF/ams_07km/recortes/grh/json2/{ano}/{mes}/{dia}/00/{municipio}.json"
			)
	) |>
		as.character()

	resposta <- httr2::request(link) |>
		httr2::req_error(is_error = \(resp) FALSE) |>
		httr2::req_perform()

	if (httr2::resp_status(resposta) == 200) {
		data_df_2 <- resposta |>
			httr2::resp_body_string() |>
			jsonlite::fromJSON() |>
			purrr::pluck("datasets", "data", 1) |>
			dplyr::mutate(
				date = as.POSIXct(date, origin = "1970-01-01", tz = "UTC") +
					lubridate::hours(fcst),
				erro = 0,
				id_cidade = municipio,
				modelo = modelo
			)
	} else {
		data_df_2 <- tibble::tibble(
			erro = 1,
			id_cidade = municipio,
			modelo = modelo
		) |>
			dplyr::mutate(
				date = as.POSIXct(Sys.Date(), origin = "1970-01-01", tz = "UTC"),
				.before = 1
			)
	}

	return(data_df_2)
}

pega_dados_previsao <- function(municipio, previsao) {
	rj_cptec <- readRDS("rj_cptec.rds")

	municipio_id_cidade <- rj_cptec$id_cidade[rj_cptec$nome_mun == municipio]

	municipio <- URLencode(glue::glue("{municipio}, RJ"))

	link <- dplyr::case_when(
		previsao == "atual" ~
			glue::glue(
				"https://www.cptec.inpe.br/api/forecast-input?city={municipio}"
			),
		previsao == "7_dias" ~
			glue::glue(
				"https://www.cptec.inpe.br/api/forecast-input-next-days?city={municipio}"
			)
	)

	resposta <- httr2::request(link) |>
		httr2::req_error(is_error = \(resp) FALSE) |>
		httr2::req_perform()

	if (httr2::resp_status(resposta) == 200) {
		data_df <- httr2::resp_body_string(resposta) |>
			jsonlite::fromJSON() |>
			tibble::as_tibble() |>
			dplyr::mutate(
				erro = 0,
				id_cidade = municipio_id_cidade,
				cod_mun = rj_cptec$cod_mun[rj_cptec$id_cidade == municipio_id_cidade],
				modelo = previsao,
				# data_ = as.POSIXct(Sys.Date(), origin = "1970-01-01", tz = "UTC"),
				.before = 1
			)
	} else {
		data_df <- tibble::tibble(
			erro = 1,
			id_cidade = municipio_id_cidade,
			cod_mun = rj_cptec$cod_mun[rj_cptec$id_cidade == municipio_id_cidade],
			modelo = previsao
		) |>

			dplyr::mutate(
				# data_ = as.POSIXct(Sys.Date(), origin = "1970-01-01", tz = "UTC"),
				.before = 1
			)
	}

	return(data_df)
}

pega_dados_previsao_total <- function(municipio) {
	bd_atual <- pega_dados_previsao(municipio = municipio, previsao = "atual")
	bd_previsao <- pega_dados_previsao(municipio = municipio, previsao = "7_dias")

	data_referencia <- bd_atual$hora_atual |> as.Date()
	data_a_preencher <- if (is.na(data_referencia)) {
		paste0(Sys.Date() + 1:6) |> as.Date()
	} else {
		paste0(data_referencia + 1:nrow(bd_previsao)) |> as.Date()
	}

	bd_previsao <- bd_previsao |>
		dplyr::mutate(
			data_atual = as.Date(data_a_preencher)
		)

	base_total <- dplyr::bind_rows(bd_atual, bd_previsao) |>
		tidyr::fill(
			dplyr::any_of(
				c("id_cidade", "id_ibge", "latitude", "longitude", "cidade", "uf")
			),
			.direction = "down"
		) |>
		dplyr::mutate(
			data_atual = if_else(is.na(data_atual), data_referencia, data_atual),
			semana_dia = if_else(
				is.na(semana_dia),
				as.character(lubridate::wday(
					as.Date(hora_atual),
					label = T,
					abbr = TRUE
				)),
				semana_dia
			)
		)

	return(base_total)
}

# base_rio_janeiro <- purrr:::map(
#   .x = rj_cptec$nome_mun,
#   .f = pega_dados_previsao_total,
#   .progress = TRUE) |>
#   purrr::list_rbind()

# http://www3.cptec.inpe.br/dimnt/base-de-dados/previsoes-cptec/
# http://www3.cptec.inpe.br/dimnt/avaliacao-de-modelos/relatorio-de-avaliacao-dos-modelos-bam-brams-eta-wrf-e-gfs-experimento-de-janeiro-de-2021/
# http://www3.cptec.inpe.br/dimnt/wp-content/uploads/sites/3/2022/02/TABELA-CaracteristicasModelosRegionais-EXPjan2021F.pdf
# http://www3.cptec.inpe.br/dimnt/modelos-numericos/
# http://brams.cptec.inpe.br/
