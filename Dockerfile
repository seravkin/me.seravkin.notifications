FROM hseeberger/scala-sbt AS build-env

WORKDIR /app

COPY . ./
RUN sbt assembly -batch

FROM openjdk:8u181
WORKDIR /app
COPY --from=build-env /app/target/scala-2.12/me-seravkin-notifications.jar .

ENTRYPOINT ["java", "-jar", "me-seravkin-notifications.jar"]
