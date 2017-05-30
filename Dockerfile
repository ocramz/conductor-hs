FROM haskell:8.0


# NB stack is already present in the image


# COPY . /opt/servant-api
# WORKDIR /opt/servant-api
# RUN stack build
# CMD ["stack","exec","servant-api-exe"]