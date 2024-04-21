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
import de.z0rdak.yawp.commands.arguments.ArgumentUtil;
import de.z0rdak.yawp.core.region.DimensionalRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import net.minecraft.command.CommandSource;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static de.z0rdak.yawp.util.MessageSender.sendCmdFeedback;

public class AddRegionChildArgumentType implements ArgumentType<String> {

    private static final Collection<String> EXAMPLES = Stream.of(new String[]{"spawn", "arena4pvp", "shop", "nether-hub"})
            .collect(Collectors.toSet());

    private static final SimpleCommandExceptionType ERROR_AREA_INVALID = new SimpleCommandExceptionType(new TranslationTextComponent("cli.arg.region.parse.invalid"));

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            flag -> new TranslationTextComponent("cli.arg.region.invalid", flag)
    );

    public static final Pattern VALID_NAME_PATTERN = Pattern.compile("^[A-Za-z]+[A-Za-z\\d\\-]+[A-Za-z\\d]+$");

    @Override
    public String parse(StringReader reader) throws CommandSyntaxException {
        int i = reader.getCursor();
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

    /**
     * Lists possible regions which can be added as children. <br>
     * These are most likely only regions which have the dimensional region as their parent and are fully contained in the area of the parent region.
     * @param context
     * @param builder
     * @return
     * @param <S>
     */
    @SuppressWarnings("unchecked")
    @Override
    // TODO: Extend suggestions for any region and check if their parents are dim or local regions
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSource) {
            CommandSource src = (CommandSource) context.getSource();
            DimensionRegionCache dimCache = ArgumentUtil.getDimCacheArgument((CommandContext<CommandSource>) context);
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            IMarkableRegion region = ArgumentUtil.getRegionArgument((CommandContext<CommandSource>) context);
            List<String> potentialChildrenNames = dimRegion.getChildren().values()
                    .stream()
                    .map(r -> (IMarkableRegion) r)
                    .filter(r -> !r.getName().equals(region.getName()))
                    .filter(r -> (region.getArea()).containsOther(r.getArea()))
                    .map(IMarkableRegion::getName)
                    .collect(Collectors.toList());
            if (potentialChildrenNames.isEmpty()) {
                sendCmdFeedback(src, new StringTextComponent("There are no valid child regions for region '" + region.getName() + "'."));
                return Suggestions.empty();
            }
            return ISuggestionProvider.suggest(potentialChildrenNames, builder);
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    /**
     * Using this as an actual argument does not work on a server-side only mod,
     * because it needs to be registered in the corresponding registry.
     */
    public static AddRegionChildArgumentType potentialChildRegions() {
        return new AddRegionChildArgumentType();
    }
}
