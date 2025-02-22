package de.z0rdak.yawp.util;

import com.google.gson.annotations.SerializedName;
import com.mojang.authlib.GameProfile;
import com.mojang.authlib.exceptions.MinecraftClientException;
import com.mojang.authlib.exceptions.MinecraftClientHttpException;
import com.mojang.authlib.minecraft.client.ObjectMapper;
import com.mojang.authlib.yggdrasil.response.ErrorResponse;
import com.mojang.brigadier.context.CommandContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.server.players.GameProfileCache;
import org.apache.commons.io.IOUtils;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;

import static de.z0rdak.yawp.constants.Constants.LOGGER;

public class MojangApiHelper {

    private static final ObjectMapper OBJECT_MAPPER = ObjectMapper.create();

    private MojangApiHelper() {
    }

    public static void getGameProfileInfo(UUID uuid, Consumer<@Nullable GameProfile> onResult) {
        try {
            String uuidWithOutDashes = uuid.toString().replace("-", "");
            String uri = "https://api.minecraftservices.com/minecraft/profile/lookup/" + uuidWithOutDashes;
            onResult.accept(fetchGameProfileInfo(uri));
        } catch (Exception e) {
            onResult.accept(null);
            LOGGER.error("Error fetching game profile info for player '{}': {}", uuid.toString(), e);
        }
    }

    public static void getGameProfileInfo(String username, Consumer<@Nullable  GameProfile> onResult) {
        try {
            String uri = "https://api.mojang.com/users/profiles/minecraft/" + username;
            onResult.accept(fetchGameProfileInfo(uri));
        } catch (Exception e) {
            onResult.accept(null);
            LOGGER.error("Error fetching game profile info for player '{}': {}", username, e);
        }
    }

    /**
     * Referenced: {@link com.mojang.authlib.minecraft.client.MinecraftClient#readInputStream(URL, Class, HttpURLConnection)}
     */
    private static GameProfile fetchGameProfileInfo(String uri) {
        try {
            HttpURLConnection connection = createUrlConnection(new URI(uri).toURL());
            InputStream inputStream = null;
            try {
                final int status = connection.getResponseCode();

                final String result;
                if (status < 400) {
                    inputStream = connection.getInputStream();
                    result = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
                    if (result.isEmpty()) {
                        return null;
                    }
                    return OBJECT_MAPPER.readValue(result, GameProfile.class);
                } else {
                    final String contentType = connection.getContentType();
                    inputStream = connection.getErrorStream();
                    final ErrorResponse errorResponse;
                    if (inputStream != null) {
                        result = IOUtils.toString(inputStream, StandardCharsets.UTF_8);
                        if (contentType != null && contentType.startsWith("text/html")) {
                            LOGGER.error("Got an error with a html body connecting to {}: {}", uri, result);
                            throw new MinecraftClientHttpException(status);
                        }
                        errorResponse = OBJECT_MAPPER.readValue(result, ErrorResponse.class);
                        throw new MinecraftClientHttpException(status, errorResponse);
                    } else {
                        throw new MinecraftClientHttpException(status);
                    }
                }
            } catch (final IOException e) {
                //Connection errors
                throw new MinecraftClientException(
                    MinecraftClientException.ErrorType.SERVICE_UNAVAILABLE , "Failed to read from " + uri + " due to " + e.getMessage(), e);
            } finally {
                IOUtils.closeQuietly(inputStream);
            }
        } catch (URISyntaxException|MalformedURLException e) {
            LOGGER.error("Could not create URL of {}", uri);
            return null;
        }
    }

    public static Optional<GameProfile> lookupGameProfileInCache(CommandContext<CommandSourceStack> ctx, String playerName) {
        GameProfileCache profileCache = ctx.getSource().getServer().getProfileCache();
        // Uses Mojang's API to retrieve info from player repo. It invokes
        // YggdrasilGameProfileRepository.findProfilesByNames through the PlayerProfileCache
        // which itself makes an HTTP request to Mojang's API
        return profileCache.get(playerName);
    }

    public static Optional<GameProfile> lookupGameProfileInCache(CommandContext<CommandSourceStack> ctx, UUID uuid) {
        GameProfileCache profileCache = ctx.getSource().getServer().getProfileCache();
        // This in contrast to the name search does not make an HTTP request
        // It just looks up the profile in the cache
        return profileCache.get(uuid);
    }

    /**
     * Referenced: {@link com.mojang.authlib.minecraft.client.MinecraftClient#createUrlConnection(URL)}
     */
    private static HttpURLConnection createUrlConnection(URL url) {
        try {
            LOGGER.debug("Connecting to {}", url);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setConnectTimeout(5000);
            connection.setReadTimeout(5000);
            connection.setUseCaches(false);
            return connection;
        } catch (IOException io) {
            throw new MinecraftClientException(MinecraftClientException.ErrorType.SERVICE_UNAVAILABLE, "Failed connecting to " + url, io);
        }
    }
}
