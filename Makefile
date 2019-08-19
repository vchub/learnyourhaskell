# source:
# https://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html
# https://github.com/input-output-hk/cardano-sl/blob/755b3b8fa7981982e9355052be69432952dee528/wallet/Makefile
#
help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid: ## Run ghcid with the wallet-new project
	ghcid \
	    --command "stack ghci Lib --ghci-options=-fno-code"

ghcid-test: ## Have ghcid run the test suite for the wallet-new-specs on successful recompile
	ghcid \
		--command "stack ghci learnyourhaskell --ghci-options=-fobject-code" \
	    --test "D1Spec"

.PHONY: ghcid ghcid-test help
