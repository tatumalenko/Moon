FROM mcr.microsoft.com/dotnet/core/sdk:3.1

COPY Moon/bin/Release/netcoreapp3.1/publish Moon/

ENTRYPOINT ["dotnet", "Moon/Moon.dll"]