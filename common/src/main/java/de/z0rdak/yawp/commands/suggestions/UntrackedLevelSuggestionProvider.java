package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.core.RegionManager;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;

import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class UntrackedLevelSuggestionProvider implements SuggestionProvider<CommandSourceStack> {

    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> context, SuggestionsBuilder builder) {
        var allLevels = context.getSource().levels().stream().
                map(ResourceKey::identifier)
                .collect(Collectors.toSet());
        var tracked = RegionManager.get().getLevels();
        allLevels.removeAll(tracked);
        var untracked = allLevels.stream().map(Identifier::toString).toList();
        untracked.forEach(level -> builder.suggest(level, Component.literal("Track " + level)));
        return builder.buildFuture();
    }
}
