-type log_entry_header_fun()::fun((Category::string()) -> atom() | tuple()).
-type log_entry_fun()::fun((Body::string()) -> atom() | tuple()).
-type log_entry_footer_fun()::fun((Footer::string()) -> atom() | tuple()).
-record(logger, {log_entry_header::log_entry_header_fun(), log_entry::log_entry_fun(), log_entry_footer::log_entry_footer_fun()}).