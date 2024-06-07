package de.z0rdak.yawp.commands.arguments.flag;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.CuboidRegion;
import net.minecraft.command.CommandSource;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.Text;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;

public class RegionFlagArgumentType implements ArgumentType<String> {

    public static final Pattern VALID_FLAG_PATTERN = Pattern.compile("^[A-Za-z][A-Za-z\\-][A-Za-z]$");
    private static final Collection<String> EXAMPLES = RegionFlag.getFlagNames();
    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(Text.translatableWithFallback("cli.arg.flag.parse.invalid", "Unable to parse flag identifier!"));
    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> Text.translatableWithFallback("cli.arg.flag.invalid", "Invalid flag identifier: '%s'", flag)
    );

    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     */
    public static RegionFlagArgumentType flag() {
        return new RegionFlagArgumentType();
    }

    public static RegionFlag getFlag(CommandContext<ServerCommandSource> ctx, String argName) throws CommandSyntaxException {
        String flagIdentifier = ctx.getArgument(argName, String.class);
        if (RegionFlag.contains(flagIdentifier)) {
            return RegionFlag.fromId(flagIdentifier);
        } else {
            sendCmdFeedback(ctx.getSource(), Text.literal("Invalid flag identifier: '" + flagIdentifier + "'!"));
            throw ERROR_INVALID_VALUE.create(flagIdentifier);
        }
    }

    @Override
    public String parse(StringReader reader) throws CommandSyntaxException {
        int i = reader.getCursor();

        while (reader.canRead() && String.valueOf(reader.peek()).matches(Pattern.compile("^[A-Za-z\\d\\-]$").pattern())) {
            reader.skip();
        }
        String s = reader.getString().substring(i, reader.getCursor());

        try {
            boolean isValidName = s.matches(VALID_FLAG_PATTERN.pattern());
            if (isValidName) {
                return s;
            } else {
                throw new IllegalArgumentException("Invalid flag identifier supplied");
            }
        } catch (IllegalArgumentException argumentException) {
            reader.setCursor(i);
            YetAnotherWorldProtector.LOGGER.error("Error parsing flag identifier");
            throw ERROR_AREA_INVALID.createWithContext(reader);
        }
    }

    public static Set<RegionFlag> getFlags(CommandContext<ServerCommandSource> ctx, String argName) throws CommandSyntaxException {
        String flagIdentifiers = ctx.getArgument(argName, String.class);
        Set<String> flagsList = new HashSet<>(Arrays.asList(flagIdentifiers.split(" ")));
        Set<RegionFlag> regionFlags = flagsList.stream()
                .filter(flag -> {
                    if (RegionFlag.contains(flag))
                        return true;
                    else {
                        sendCmdFeedback(ctx.getSource(), Text.literal("Invalid flag identifier: '" + flag + "'!"));
                        return false;
                    }
                })
                .map(RegionFlag::fromId)
                .collect(Collectors.toSet());
        if (regionFlags.isEmpty()) {
            throw ERROR_INVALID_VALUE.create(flagIdentifiers);
        }
        return regionFlags;
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    // TODO: Replace usages with IFlagArgumentType where possible
    @Override
    @SuppressWarnings("unchecked")
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> ctx, SuggestionsBuilder builder) {
        if (ctx.getSource() instanceof ServerCommandSource src) {
            CuboidRegion region = (CuboidRegion) ArgumentUtil.getRegionArgument((CommandContext<ServerCommandSource>) ctx);
            List<String> flagNames = RegionFlag.getFlagNames();

            String input = ctx.getInput();
            if (input.contains("add")) {
                flagNames = flagNames.stream()
                        .filter(flagName -> !region.containsFlag(flagName))
                        .collect(Collectors.toList());
            }
            if (input.contains("remove")) {
                flagNames = flagNames.stream()
                        .filter(region::containsFlag)
                        .collect(Collectors.toList());
            }
            if (flagNames.isEmpty()) {
                if (input.contains("add")) {
                    sendCmdFeedback(src, Text.literal("There are no flag left to add for this region '" + region.getName() + "'."));
                }
                if (input.contains("remove")) {
                    sendCmdFeedback(src, Text.literal("Region '" + region.getName() + "' does not contain any flags."));
                }
                return Suggestions.empty();
            }
            return CommandSource.suggestMatching(flagNames, builder);
        } else {
            return Suggestions.empty();
        }
    }
}
