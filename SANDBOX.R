api_data$NDC |> unique() |> sample(100) |> stri_length() |> table()
api_data$NDC |> unique() |> sample(100) |> stri_sub(length = 1) |> table()
drug_table(openFDA_ndc$results)

replicate(
  n = 30
  , { inspect$product_ndc |> unique() |> sample(100) |> stri_replace_all_regex("-", "", vectorise_all = FALSE) |> 
      stri_length() |> table() |> t()
    })

inspect$product_ndc |> unique() |> sample(100) |> stri_replace_all_regex("-", "", vectorise_all = FALSE) |> stri_sub(length = 1) |> table()

modify_at(api_data, "NDC", \(x) stri_pad_left(x, width = 8, pad = "0"))[
  inspect[1:10] |> modify_at("product_ndc", \(x) stri_replace_all_regex(x, "-", "", vectorise_all = FALSE))
  , on = "NDC==product_ndc"
  , nomatch = NULL
  ]
