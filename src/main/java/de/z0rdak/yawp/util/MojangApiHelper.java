package de.z0rdak.yawp.util;

import com.google.gson.Gson;
import com.google.gson.annotations.SerializedName;
import com.mojang.authlib.GameProfile;
import com.mojang.brigadier.context.CommandContext;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraft.command.CommandSource;
import net.minecraft.server.management.PlayerProfileCache;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;

import javax.annotation.Nullable;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.UUID;
import java.util.function.Consumer;

import static java.nio.charset.StandardCharsets.UTF_8;

public class MojangApiHelper {

    private MojangApiHelper() {
    }

    @Nullable
    public static GameProfile getGameProfileInfo(UUID uuid, Consumer<GameProfile> onResult) {
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            String uuidWithOutDashes = uuid.toString().replace("-", "");
            String uri = "https://sessionserver.mojang.com/session/minecraft/profile/" + uuidWithOutDashes;
            HttpGet httpGet = new HttpGet(uri);
            try {
                CloseableHttpResponse response = httpclient.execute(httpGet);
                GameProfile gameProfile = deserializeGameProfile(response);
                if (gameProfile != null) {
                    return gameProfile;
                }
                onResult.accept(null);
                return null;
            } catch (IOException e) {
                YetAnotherWorldProtector.LOGGER.error("Error fetching game profile info for player '{}': {}", uuid.toString(), e);
                throw e;
            }
        } catch (IOException e) {
            onResult.accept(null);
            YetAnotherWorldProtector.LOGGER.error("Error fetching game profile info for player '{}': {}", uuid.toString(), e);
            return null;
        }
    }

    @Nullable
    private static GameProfile deserializeGameProfile(CloseableHttpResponse response) throws IOException {
        if (response.getStatusLine().getStatusCode() == 200) {
            Reader reader = new InputStreamReader(response.getEntity().getContent(), UTF_8);
            MojangProfileResponse profileResponse = new Gson().fromJson(reader, MojangProfileResponse.class);
            String uuidStr = profileResponse.id.replaceAll(
                    "(\\w{8})(\\w{4})(\\w{4})(\\w{4})(\\w{12})", "$1-$2-$3-$4-$5");
            return new GameProfile(UUID.fromString(uuidStr), profileResponse.name);
        }
        return null;
    }

    @Nullable
    public static GameProfile getGameProfileInfo(String username, Consumer<GameProfile> onResult) {
        try (CloseableHttpClient httpclient = HttpClients.createDefault()) {
            String uri = "https://api.mojang.com/users/profiles/minecraft/" + username;
            HttpGet httpGet = new HttpGet(uri);
            try {
                CloseableHttpResponse response = httpclient.execute(httpGet);
                if (response.getStatusLine().getStatusCode() == 204) {
                    // if status code is 204, then the player does not exist
                    YetAnotherWorldProtector.LOGGER.error("Could not retrieve game profile for player " + username);
                    return null;
                }
                GameProfile gameProfile = deserializeGameProfile(response);
                if (gameProfile != null) {
                    return gameProfile;
                }
                onResult.accept(null);
                return null;
            } catch (IOException e) {
                YetAnotherWorldProtector.LOGGER.error("Error fetching game profile info for player '{}': {}", username, e);
                throw e;
            }
        } catch (IOException e) {
            YetAnotherWorldProtector.LOGGER.error("Error fetching game profile info for player '{}': {}", username, e);
            onResult.accept(null);
            return null;
        }
    }

    @Nullable
    public static GameProfile lookupGameProfileInCache(CommandContext<CommandSource> ctx, String playerName) {
        PlayerProfileCache profileCache = ctx.getSource().getServer().getProfileCache();
        // Uses Mojang's API to retrieve info from player repo. It invokes
        // YggdrasilGameProfileRepository.findProfilesByNames through the PlayerProfileCache
        // which itself makes an HTTP request to Mojang's API
        return profileCache.get(playerName);
    }

    @Nullable
    public static GameProfile lookupGameProfileInCache(CommandContext<CommandSource> ctx, UUID uuid) {
        PlayerProfileCache profileCache = ctx.getSource().getServer().getProfileCache();
        // This in contrast to the name search does not make an HTTP request
        // It just looks up the profile in the cache
        return profileCache.get(uuid);
    }

    private static class MojangProfileResponse {
        @SerializedName("name")
        public String name;
        @SerializedName("id")
        public String id;
    }
}
