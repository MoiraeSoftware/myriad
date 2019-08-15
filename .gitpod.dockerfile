FROM gitpod/workspace-dotnet:latest

RUN dotnet tool install paket -g && dotnet tool install fake-cli -g
ENV PATH=~/.dotnet/tools:$PATH