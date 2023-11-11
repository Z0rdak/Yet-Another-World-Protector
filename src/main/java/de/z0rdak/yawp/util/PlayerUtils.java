package de.z0rdak.yawp.util;

import com.mojang.util.UUIDTypeAdapter;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;

import java.io.IOException;
import java.util.UUID;

public class PlayerUtils {

    private static final String MOJANG_API_URL = "https://api.mojang.com/users/profiles/minecraft/";

    private static UUID getPlayerUUIDFromMojang(String playerName) {
        try {
            HttpClient httpClient = HttpClientBuilder.create().build();
            HttpGet request = new HttpGet(MOJANG_API_URL + playerName);
            HttpResponse response = httpClient.execute(request);
            if (response.getStatusLine().getStatusCode() == 200) {
                String responseBody = EntityUtils.toString(response.getEntity());
                return parsePlayerUUIDFromJson(responseBody);
            } else {
                YetAnotherWorldProtector.LOGGER.warn("Error: " + response.getStatusLine().getStatusCode() + " - " + response.getStatusLine().getReasonPhrase());
                return null;
            }
        } catch (IOException e) {
            YetAnotherWorldProtector.LOGGER.warn("Error: " + e.getMessage());
            return null;
        }
    }

    private static UUID parsePlayerUUIDFromJson(String json) {
        // Parse the JSON response to obtain the player's UUID
        // This assumes the JSON structure contains a "id" field
        // and uses the UUIDTypeAdapter from Mojang's libraries
        // You may need to adjust this based on the actual JSON structure
        return UUIDTypeAdapter.fromString(json.substring(json.indexOf("\"id\":\"") + 6, json.indexOf("\",\"name\":\"")));
    }
}
