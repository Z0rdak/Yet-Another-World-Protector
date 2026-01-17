package de.z0rdak.yawp.commands.arguments;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.data.region.LevelRegionData;
import de.z0rdak.yawp.data.region.RegionDataManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.Identifier;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;

public class LevelRegionDataArgumentType implements ArgumentType<LevelRegionData> {

    private static final Collection<String> EXAMPLES = RegionDataManager.getLevelNames();

    private static final DynamicCommandExceptionType ERROR_INVALID_VALUE = new DynamicCommandExceptionType(
            dim -> Component.translatableWithFallback("cli.arg.dim.invalid", "Dimension '%s' does not exist!", dim)
    );

    public static LevelRegionDataArgumentType levelData() {
        return new LevelRegionDataArgumentType();
    }

    public static LevelRegionData getDimRegion(CommandContext<CommandSourceStack> context, String dim) throws CommandSyntaxException {
        Identifier levelRl = context.getArgument(dim, Identifier.class);
        boolean isValidDimIdentifier = context.getSource().levels().stream()
                .map(ResourceKey::identifier)
                .anyMatch(loc -> loc.equals(levelRl));
        if (isValidDimIdentifier) {
            // TODO: this just creates new lrd, fix it. Init new ones by command to make them available?
            LevelRegionData dimCache = RegionDataManager.getOrCreate(levelRl);
            if (dimCache == null) {
                throw ERROR_INVALID_VALUE.create(levelRl.toString());
            }
            return dimCache;
        } else {
            throw ERROR_INVALID_VALUE.create(levelRl.toString());
        }
    }

    @Override
    public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
        if (context.getSource() instanceof CommandSourceStack) {
            return SharedSuggestionProvider.suggest(RegionDataManager.getLevelNames(), builder);
        } else {
            return Suggestions.empty();
        }
    }

    @Override
    public Collection<String> getExamples() {
        return EXAMPLES;
    }

    @Override
    public LevelRegionData parse(StringReader reader) throws CommandSyntaxException {
        Identifier levelRl = Identifier.read(reader);
        if (RegionDataManager.hasLevel(levelRl)) {
            return RegionDataManager.getOrCreate(levelRl);
        }
        return null;
    }
}
