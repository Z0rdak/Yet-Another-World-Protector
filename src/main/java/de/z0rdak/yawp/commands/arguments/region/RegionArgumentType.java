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
import de.z0rdak.yawp.util.CommandUtil;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RegionArgumentType implements ArgumentType<String> {

    private static final Collection<String> EXAMPLES = Stream.of(new String[]{"spawn", "arena4pvp", "shop", "nether-hub"})
            .collect(Collectors.toSet());

    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(new TranslatableComponent("cli.arg.region.parse.invalid"));

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> new TranslatableComponent("cli.arg.region.invalid", flag)
    );

    public static final Pattern VALID_NAME_PATTERN = Pattern.compile("^[A-Za-z]+[A-Za-z\\d\\-]+[A-Za-z\\d]+$");

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

    @SuppressWarnings("unchecked")
    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSourceStack) {
            CommandSourceStack src = (CommandSourceStack) context.getSource();
            DimensionRegionCache dimCache = CommandUtil.getDimCacheArgument((CommandContext<CommandSourceStack>) context);
            Collection<String> regionNames = dimCache.getRegionNames();
            if (regionNames.isEmpty()) {
                MessageUtil.sendCmdFeedback(src, new TextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
                return Suggestions.empty();
            }
            return SharedSuggestionProvider.suggest(regionNames, builder);
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    public static RegionArgumentType region() {
        return new RegionArgumentType();
    }

    public static IMarkableRegion getRegion(CommandContext<CommandSourceStack> context, String argName) throws CommandSyntaxException {
        String regionName = context.getArgument(argName, String.class);
        // FIXME: crashes without dim argument
        DimensionRegionCache dimCache = CommandUtil.getDimCacheArgument(context);
        IMarkableRegion region = dimCache.getRegion(regionName);
        if (region != null) {
            return region;
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), new TextComponent("No regions defined in dim '" + dimCache.dimensionKey().location() + "'"));
            throw ERROR_INVALID_VALUE.create(regionName);
        }
    }
}
