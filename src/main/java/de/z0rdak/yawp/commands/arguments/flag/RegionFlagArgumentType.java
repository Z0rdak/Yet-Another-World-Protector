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
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.CuboidRegion;
import de.z0rdak.yawp.util.CommandUtil;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraft.text.LiteralTextContent;
import net.minecraft.text.MutableText;
import net.minecraft.text.TranslatableTextContent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

// TODO: When flag types are implemented there must be different ArgumentTypes for each flag type
public class RegionFlagArgumentType implements ArgumentType<String> {

    public static final Pattern VALID_FLAG_PATTERN = Pattern.compile("^[A-Za-z][A-Za-z\\-][A-Za-z]$");
    private static final Collection<String> EXAMPLES = RegionFlag.getFlagNames();
    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(MutableText.of(new TranslatableTextContent("cli.arg.flag.parse.invalid")));
    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> MutableText.of(new TranslatableTextContent("cli.arg.flag.invalid", flag))
    );

    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     */
    public static RegionFlagArgumentType flag() {
        return new RegionFlagArgumentType();
    }

    public static RegionFlag getFlag(CommandContext<ServerCommandSource> context, String argName) throws CommandSyntaxException {
        String flagIdentifier = context.getArgument(argName, String.class);
        if (RegionFlag.contains(flagIdentifier)) {
            return RegionFlag.fromId(flagIdentifier);
        } else {
            MessageUtil.sendCmdFeedback(context.getSource(), MutableText.of(new LiteralTextContent("Invalid flag identifier: '" + flagIdentifier + "'!")));
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

    @Override
    @SuppressWarnings("unchecked")
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof ServerCommandSource src) {
            try {
                CuboidRegion region = (CuboidRegion) CommandUtil.getRegionArgument((CommandContext<ServerCommandSource>) context);
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
                        MessageUtil.sendCmdFeedback(src, MutableText.of(new LiteralTextContent("There are no flag left to add for this region '" + region.getName() + "'.")));
                    }
                    if (input.contains("remove")) {
                        MessageUtil.sendCmdFeedback(src, MutableText.of(new LiteralTextContent("Region '" + region.getName() + "' does not contain any flags.")));
                    }
                    return Suggestions.empty();
                }
                return CommandSource.suggestMatching(flagNames, builder);
            } catch (CommandSyntaxException e) {
                throw new RuntimeException(e);
            }
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }
}
