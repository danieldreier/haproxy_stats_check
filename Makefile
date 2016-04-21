NAME=haproxy_check
VERSION=0.0.2

.PHONY: package
package:
	rm -f haproxy_check
	mix deps.get
	mix escript.build
	fpm --depends erlang -s dir -t rpm -n $(NAME) -v $(VERSION) --prefix /usr/local/bin haproxy_check
	fpm --depends erlang -s dir -t deb -n $(NAME) -v $(VERSION) --prefix /usr/local/bin haproxy_check
