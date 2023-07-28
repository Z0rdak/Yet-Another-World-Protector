package de.z0rdak.yawp.commands.arguments.region;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.CommandUtil;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.text.Text;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RegionArgumentType implements ArgumentType<String> {

    public static final Pattern VALID_NAME_PATTERN = Pattern.compile("^[A-Za-z]+[A-Za-z\\d\\-]+[A-Za-z\\d]+$");
    private static final Collection<String> EXAMPLES = Stream.of(new String[]{"spawn", "arena4pvp", "shop", "nether-hub"})
            .collect(Collectors.toSet());
    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(
            Text.translatableWithFallback("cli.arg.region.parse.invalid", "Unable to parse region name!"));

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> Text.translatableWithFallback("cli.arg.region.invalid", "Region '%s' does not exist", flag)
    );

    public static IMarkableRegion getRegion(CommandContext<ServerCommandSource> context, String argName) throws CommandSyntaxException {
        String regionName = context.getArgument(argName, String.class);
        DimensionRegionCache dimCache = CommandUtil.getDimCacheArgument(context);
        if (!dimCache.contains(regionName)) {
            MessageUtil.sendCmdFeedback(context.getSource(), Text.literal("No region with name '" + regionName + "' defined in dim '" + dimCache.dimensionKey().getValue() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), Text.literal(("No regions defined in dim '" + dimCache.dimensionKey().getValue() + "'")));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }

    public static RegionArgumentType region() {
        return new RegionArgumentType();
    }

    public static IMarkableRegion getRegionInPlayerDim(CommandContext<ServerCommandSource> context, String argName) throws CommandSyntaxException {
        String regionName = context.getArgument(argName, String.class);
        ServerPlayerEntity player = context.getSource().getPlayerOrThrow();
        DimensionRegionCache dimCache = RegionDataManager.get().cacheFor(player.world.getRegistryKey());
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), Text.literal(("No regions defined in dim '" + dimCache.dimensionKey().getValue() + "'")));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }

    @Override
    public String parse(StringReader reader) throws CommandSyntaxException {
        int i = reader.getCursor();

        // FIXME: Pattern only matches chars, not the valid name
        while (reader.canRead() && String.valueOf(reader.peek()).matches(Pattern.compile("^[A-Za-z\\d\\-]$").pattern())) {
            reader.skip();
        }
        String s = reader.getString().substring(i, reader.getCursor());

        try {
            boolean isValidName = s.matches(VALID_NAME_PATTERN.pattern());
            if (isValidName) {
                return s;
            } else {
                throw new IllegalArgumentException("Invalid region name supplied");
            }
        } catch (IllegalArgumentException argumentException) {
            reader.setCursor(i);
            YetAnotherWorldProtector.LOGGER.error("Error parsing region name");
            throw ERROR_AREA_INVALID.createWithContext(reader);
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    @SuppressWarnings("unchecked")
    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof ServerCommandSource src) {
            try {
                DimensionRegionCache dimCache = CommandUtil.getDimCacheArgument((CommandContext<ServerCommandSource>) context);
                Collection<String> regionNames = dimCache.getRegionNames();
                if (regionNames.isEmpty()) {
                    MessageUtil.sendCmdFeedback(src, Text.literal(("No regions defined in dim '" + dimCache.dimensionKey().getValue() + "'")));
                    return Suggestions.empty();
                }
                return CommandSource.suggestMatching(regionNames, builder);
            } catch (CommandSyntaxException e) {
                return Suggestions.empty();
            }
        } else {
            return Suggestions.empty();
        }
    }
}
