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
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class RegionFlagArgumentType implements ArgumentType<String> {

    private static final Collection<String> EXAMPLES = RegionFlag.getFlagNames();

    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(new TranslationTextComponent("cli.arg.flag.parse.invalid"));

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> new TranslationTextComponent("cli.arg.flag.invalid", flag)
    );

    public static final Pattern VALID_FLAG_PATTERN = Pattern.compile("^[A-Za-z][A-Za-z\\-][A-Za-z]$");

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

    public static Set<RegionFlag> getFlags(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String flagIdentifiers = context.getArgument(argName, String.class);
        Set<String> flagsList = new HashSet<>(Arrays.asList(flagIdentifiers.split(" ")));
        Set<RegionFlag> regionFlags = flagsList.stream()
                .filter(flag -> {
                    if (RegionFlag.contains(flag))
                        return true;
                    else {
                        MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("Invalid flag identifier: '" + flag + "'!"));
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

    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     */
    public static RegionFlagArgumentType flag() {
        return new RegionFlagArgumentType();
    }

    public static RegionFlag getFlag(CommandContext<CommandSource> context, String argName) throws CommandSyntaxException {
        String flagIdentifier = context.getArgument(argName, String.class);
        if (RegionFlag.contains(flagIdentifier)) {
            return RegionFlag.fromId(flagIdentifier);
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), new StringTextComponent("Invalid flag identifier: '" + flagIdentifier + "'!"));
            throw ERROR_INVALID_VALUE.create(flagIdentifier);
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    // TODO: Replace usages with IFlagArgumentType where possible
    @Override
    @SuppressWarnings("unchecked")
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSource) {
            CommandSource src = (CommandSource) context.getSource();
            CuboidRegion region = (CuboidRegion) ArgumentUtil.getRegionArgument((CommandContext<CommandSource>) context);
            List<String> flagNames = RegionFlag.getFlagNames();

            String input = context.getInput();
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
                    MessageUtil.sendCmdFeedback(src, new StringTextComponent("There are no flag left to add for this region '" + region.getName() + "'."));
                }
                if (input.contains("remove")) {
                    MessageUtil.sendCmdFeedback(src, new StringTextComponent("Region '" + region.getName() + "' does not contain any flags."));
                }
                return Suggestions.empty();
            }
            return ISuggestionProvider.suggest(flagNames, builder);
        } else {
            return Suggestions.empty();
        }
    }
}
