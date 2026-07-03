package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;

import java.util.concurrent.CompletableFuture;

import static de.z0rdak.yawp.api.commands.CommandConstants.LEVEL;

public class LevelRegionSuggestionProvider implements SuggestionProvider<CommandSourceStack> {

    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> ctx, SuggestionsBuilder builder) {
        Identifier levelId = ctx.getArgument(LEVEL.toString(), Identifier.class);

        ResourceKey<Level> key =
                ResourceKey.create(Registries.DIMENSION, levelId);

        var levelData = RegionManager.get().getLevelRegionData(key);

        if (levelData.isEmpty()) {
            return Suggestions.empty();
        }

        levelData.get().getLocalList().stream()
                .map(IMarkableRegion::getName)
                .forEach(builder::suggest);

        return builder.buildFuture();
    }

    public static CompletableFuture<Suggestions> suggest(CommandContext<CommandSourceStack> ctx, SuggestionsBuilder builder) {
        return new LevelRegionSuggestionProvider().getSuggestions(ctx, builder);
    }
}