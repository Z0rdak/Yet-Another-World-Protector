package de.z0rdak.yawp.commands.suggestions;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.core.region.GlobalRegion;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Entity;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

public class RegionSuggestionProvider implements SuggestionProvider<CommandSourceStack> {

    public static final Set<String> regionIdentifiers = new HashSet<>();
    public static final Set<Identifier> regionRlIdentifiers = new HashSet<>();

    @Override
    public CompletableFuture<Suggestions> getSuggestions(CommandContext<CommandSourceStack> context, SuggestionsBuilder builder) {
        Entity entity = context.getSource().getEntity();
        var level = entity != null ? entity.level() : context.getSource().getLevel();
        var maybeLrd = RegionManager.get().getLevelRegionData(level.dimension());
        maybeLrd.ifPresent(lrd -> {
            var quotedRegionNames = lrd.getLocalNames().stream().map(rn -> lrd.getId().toString() + "/" + rn).toList();
            quotedRegionNames.forEach(builder::suggest);
        });
        //TODO: local first, only prefix non unique locals
        //need a table to do this effectively
        RegionManager.get().getLevelNames().forEach(builder::suggest);
        builder.suggest(GlobalRegion.GLOBAL.toString(), Component.literal("YAWPs global region which spans all levels."));

        return builder.buildFuture();

    }
}
