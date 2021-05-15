FROM hseeberger/scala-sbt:11.0.10_1.5.2_2.13.5 AS build-env

WORKDIR /app

COPY ./project/build.properties ./project/
COPY ./project/plugins.sbt ./project/
COPY ./build.sbt ./

RUN sbt update

COPY . ./

RUN sbt assembly -batch

FROM openjdk:8u181
WORKDIR /app
COPY --from=build-env /app/target/scala-2.12/me-seravkin-notifications.jar .

ENTRYPOINT ["java", "-jar", "me-seravkin-notifications.jar"]
